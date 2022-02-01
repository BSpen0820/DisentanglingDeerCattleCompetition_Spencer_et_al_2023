#Title: Deer GPS Prep
#Bryan Spencer


#Load Libraries
library(foreign)
library(tidyverse)
library(lubridate)
library(sf)

########################GPS Data Preparation########################################

#Read in the Raw GPS Data
GPSFiles <- list.files("./Data/DBF Files", full.names = T) #list the files with full directoies for easy read in

RawGPS <- lapply(GPSFiles, FUN = read.dbf)  #Read in the files into a list
names(RawGPS) <- substr(GPSFiles, 28,32) #Assign the collar serial numbers as the names to the Raw GPS list

RawGPS[[1]] #look at the first collars data

#Matching Collar Serial Number Data to Animal ID will help when we merge the list into a single data frame.
AniID <- read.csv("./Data/DeerID.csv", header = T) #read in the csv that matches the collar serial number to the animal ID

for (x in 1:length(RawGPS)) { #simple for loop that use the collar serial number to assign a column the Animal ID
  sn <- names(RawGPS[x])
  id <- AniID[AniID$Collar.SN. == sn, 3]
  RawGPS[[x]]$ID <- id
}

RawGPS[[1]] #look at the first collars data

#Convert the Raw GPS List to a Data Frame
AllGPS <- data.frame(matrix(nrow = 0, ncol = 44)) #Creates an empty data frame to merge all the GPS data into

names(AllGPS) <- names(RawGPS[[1]]) #Copy's the column names of the list data frames into the combined data frame

#Simple for loop to copy the data frame from the list into the combined data frame
for (x in 1:length(RawGPS)) { 
  AllGPS <- rbind(AllGPS, RawGPS[[x]])
}

rm(RawGPS)  

#filter the GPS data to the time period of interest: a month before cattle were stocked to the day before the collars were collected
AllGPS <- AllGPS %>% filter(GMT_DATE >= (as.Date("2020-11-05", "%Y-%m-%d") - days(30))) %>%
  filter(GMT_DATE <= (as.Date("2020-11-16", "%Y-%m-%d") + days(30)))


#Label Each point as either before, after, or during the stocking event
for (i in 1:nrow(AllGPS)){
  if (AllGPS$GMT_DATE[i] < as.Date("2020-11-05", "%Y-%m-%d")){AllGPS$StStatus[i] <- "before"} else
  if (AllGPS$GMT_DATE[i] > as.Date("2020-11-16", "%Y-%m-%d")){AllGPS$StStatus[i] <- "after"} else
  {AllGPS$StStatus[i] <- "during"}
}

#Remove all the row labeled during, since it won't be analysed
AllGPS <- AllGPS %>% filter(AllGPS$StStatus != "during")

#Create a new column in which we can merge the date and time together for POSIX time for further analysis and use
AllGPS$POSTime <- paste(AllGPS$GMT_DATE, " ", AllGPS$GMT_TIME)
AllGPS$POSTime <- as.POSIXct(AllGPS$POSTime, tz = "America/Chicago", format = "%Y-%m-%d %H:%M:%OS")

write.csv(AllGPS, "./Output/GPS Data Prep/GPSData.csv", row.names = F)

#######################Linking Stocking Data to the Point Data################

Colo.Area <- read_sf("./Output/Coloraditas Pastures/GrazingUnit.shp") #Read in Coloraditas Area ShapeFile

plot(st_geometry(Colo.Area)) #plot it

Colo.Area$hect <- as.numeric(st_area(Colo.Area)/10000) #Calculate the area of the pastures in hectares

Stock.Data <- read.csv("./Data/StockingDensityTotal.csv", header = T) #read in stocking data 

Stock.Data <- separate(Stock.Data, Destination, into = c("GrazingUnit", "Pasture"), sep = ": ") #split the grazing unit and pastures into separate columns

#Short for loop to calculate the stocking density in auy/ha
for(i in 1:length(Stock.Data$Pasture)) {
  Stock.Data$Stock.Dens[i] <- Stock.Data$Total[i]/Colo.Area$hect[Colo.Area$Name == Stock.Data$Pasture[i]]
}

AllGPS <- read.csv("./Output/GPS Data Prep/GPSData.csv", header = T) #read in GPS point data
AllGPS.SF <- st_as_sf(AllGPS, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, na.fail = F, remove = F) #convert it to SF object
AllGPS.SF <- st_transform(AllGPS.SF, crs = st_crs(Colo.Area)) #reproject it to the matching crs
AllGPS.SF$LINE_NO <- 1:nrow(AllGPS.SF)
plot(st_geometry(AllGPS.SF), add = T) #See if the points fall within the shapefile pastures
#st_write(AllGPS.SF, "./Output/GPS Data Prep/GPSloc.shp")

AllGPS.Cont <- st_intersection(AllGPS.SF, Colo.Area)
Keep <- names(AllGPS.SF)
Keep <- append(Keep, "Name")
AllGPS.Cont <- AllGPS.Cont[,Keep]

AllGPS.SF$Name <- NA
AllGPS <- rbind(AllGPS.Cont, AllGPS.SF)
AllGPS <- AllGPS[!duplicated(AllGPS$LINE_NO),]
rm(AllGPS.Cont)
rm(AllGPS.SF)

plot(st_geometry(Colo.Area))
plot(st_geometry(AllGPS), add = T)

Stock.Data$Date <- as.Date(Stock.Data$Date, "%m/%d/%Y")
AllGPS$GMT_DATE <- as.Date(AllGPS$GMT_DATE, "%Y-%m-%d")
AllGPS <- AllGPS[order(AllGPS$LINE_NO),]
st_write(AllGPS, "./Output/GPS Data Prep/GPSLocFinal.shp")

for (i in 1:nrow(AllGPS)) {
  if (is.na(AllGPS$Name[i])){AllGPS$Stock.Dens[i] <- 0} else
  if (AllGPS$GMT_DATE[i] < as.Date("2020-11-16")) {AllGPS$Stock.Dens[i] <- 0} else
  if (AllGPS$GMT_DATE[i] > as.Date("2020-11-16")) {AllGPS$Stock.Dens[i] <- Stock.Data$Stock.Dens[AllGPS$Name[i] == Stock.Data$Pasture]}
  }

AllGPS <- st_drop_geometry(AllGPS)
write.csv(AllGPS, "./Output/GPS Data Prep/AllGPSAndStockingDens.csv", row.names = F)

#######################Filter out Deer that never experienced stocking event##############

rm(list = ls())

AllGPS <- read.csv("./Output/GPS Data Prep/AllGPSAndStockingDens.csv", header = T)
StockInfo <- aggregate(Stock.Dens ~ ID + StStatus, data = AllGPS, FUN = "mean")
Deer.Remove <- StockInfo$ID[StockInfo$Stock.Dens == 0 & StockInfo$StStatus == "after"]

AllGPS <- AllGPS %>% filter(ID != Deer.Remove[1]) %>% filter(ID != Deer.Remove[2]) %>% filter(ID != Deer.Remove[3])

write.csv(AllGPS, "./FinalAnalysisData/DeerGPSData.csv", row.names = F)

#######################GPS Data Bounding Box #################################

rm(list = ls())

AllGPS <- read.csv('./FinalAnalysisData/DeerGPSData.csv', header = T)
AllGPS <- AllGPS[!is.na(AllGPS$LONGITUDE), ]

AllGPS<- st_as_sf(AllGPS, coords = c('LONGITUDE', 'LATITUDE'), crs = 4326)
AllGPS<- st_transform(AllGPS, crs = 26914)
plot(st_geometry(AllGPS))

BBox <- st_bbox(AllGPS)
BBox[1] <- BBox[1] - 1000
BBox[2] <- BBox[2] - 1000
BBox[3] <- BBox[3] + 1000
BBox[4] <- BBox[4] + 1000

BBox <- st_as_sfc(BBox)

plot(BBox, add = T)

write_sf(BBox, "./Output/BoundingBox/deerbbox.shp", overwrite = T)

