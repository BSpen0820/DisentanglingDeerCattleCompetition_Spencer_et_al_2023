#Title: Home Range Analysis Presence vs Absence
#By: Bryan Spencer
#Date: 01/30/2022

#######Read in Libraries###################

library(tidyverse)
library(raster)
library(sf)
library(sp)
library(move)
library(lme4)
library(lmerTest)

##########Home Ranges before and After###########

Deer <- read.csv("./Output/GPS Data Prep/AllGPSAndStockingDens.csv", header = T) #read in deer GPS data
Deer$POSTime <- as.POSIXct(Deer$POSTime)
Ind.ID <- unique(Deer$ID)
Case <- unique(Deer$StStatus)

rast <- raster("./FinalAnalysisData/PercentWoodyCover.tif")
rast <- projectRaster(rast, crs = CRS('+proj=aeqd'))

#Create an empty dataframe to store homerange data
HR.table <- data.frame(matrix(nrow = 0, ncol = 5))
names(HR.table) <- c("ID", "Status", "Type", "Area", "StRate")

#Dynamic Browning Bridge Loop
for (x in 1:19) {
  x <- 12
  
  ind.df <- Deer %>% filter(ID == Ind.ID[x]) #filter to an individual
 
  for(i in 1:2){
    i <- 1
    
  
    ind.df.st <- ind.df %>% filter(StStatus == Case[i])
    
    
    M.Deer <- move(x = ind.df.st$LONGITUDE, y = ind.df.st$LATITUDE, 
                   animal = ind.df.st$ID, time = ind.df.st$POSTime, data = ind.df.st, 
                   proj= CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), sensor = "GPS")
    
    M.Deer <- spTransform(M.Deer, CRSobj = "+proj=aeqd") #Transform GPS Data into AEQD projection
    
    #Create DBBMM for an individual deer
    dbbmm <- brownian.bridge.dyn(object=M.Deer, location.error=10, margin=5,
                                     window.size=21, raster= rast) 
    
    r2c <- raster2contour(dbbmm, levels = c(.95, .50))  # turns dbbmm object into contour lines
    
    IsoSF <- st_as_sf(r2c) #Converts the countor lines into an sf object
    
    IsoSTR <- st_cast(IsoSF, to = "MULTILINESTRING") #Converts contour lines into sf multilinestring
    
    IsoPoly <- st_cast(IsoSTR, to = "MULTIPOLYGON") #Converts multilinestring to MultiPolygon
    
    IsoPoly$Area <- as.numeric(abs(st_area(IsoPoly)/10000)) #Calculates the polygon area in hectares
    
    IsoPoly$ID <- ind.df.st$ID[x] #Copy the Deer ID
    
    IsoPoly$Type <- ifelse(IsoPoly$level == "0.5", "Core", "HR") #Add whether it the core area or homerange
    
    IsoPoly$Status <- unique(ind.df.st$StStatus) #Copy the stocking status
    
    IsoPoly$StRate <- mean(ind.df.st$Stock.Dens)
    
    Info <- st_drop_geometry(IsoPoly[,c(4,6,5,3,7)]) #Copy into format to add to empty HR table
    
    HR.table <- rbind(HR.table, Info) #Add the Id, Date, Isopleth level, and Area to HR table
  }
}

write.csv(HR.table, "./FinalAnalysisOutput/HRData.csv", row.names = F) #Save

#######################Analysis of Home Ranges###############################

HR.table <- read.csv("./FinalAnalysisOutput/HRData.csv", header = T)
HR.table$Status <- factor(HR.table$Status)

HR <- HR.table %>% filter(Type == "HR")

HR.model <- lmer(Area ~ Status + (1|ID), data = HR)
HRModel <- summary(HR.model)

CA <- HR.table %>% filter(Type == "Core")

CA.model <- lmer(Area ~ Status + (1|ID), data = CA)
CAModel <- summary(CA.model)

save(list = c("Out", "Out2"), file = "./FinalAnalysisOutput/HRModelSummary.RData")

