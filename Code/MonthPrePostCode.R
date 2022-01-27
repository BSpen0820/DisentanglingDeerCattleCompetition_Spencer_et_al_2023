#Title: Deer Behavior Month Pre and Post Cattle Deployment
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

#Create a new column in which we can merge the date and time together for POSIX time for further analysis and use
AllGPS$POSTime <- paste(AllGPS$GMT_DATE, " ", AllGPS$GMT_TIME)
AllGPS$POSTime <- as.POSIXct(AllGPS$POSTime, tz = "America/Chicago", format = "%Y-%m-%d %H:%M:%OS")

write.csv(AllGPS, "./Output/GPS Data Prep/GPSData.csv", row.names = F)

#######################GPS Data Bounding Box #################################

AllGPS <- read.csv('./Output/GPS Data Prep/GPSData.csv', header = T)
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

write_sf(BBox, "./Output/BoundingBox/deerbbox.shp")

#######################Bounding out and preping Raster Stack#################

