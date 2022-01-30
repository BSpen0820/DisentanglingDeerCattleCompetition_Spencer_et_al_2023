#Title: Spatial Variables Prep
#By: Bryan Spencer
#Date: 1/30/2022

##############Read in Libraries###########

library(tidyverse)
library(sf)
library(raster)

##############Woody Cover Image###########

Woody.Rast <- raster("./Data/ColoClassedImage.tif") #Read in Woody Cover Raster Layer
Woody.Rast #Checking the resoltion and crs
plot(Woody.Rast) #Plot

Deer.bbox <- st_read("./Output/BoundingBox/deerbbox.shp") #Read in Deer BBox with the 1km buffer
Deer.bbox #Checking the crs to make sure it matches the woody raster layer
plot(st_geometry(Deer.bbox), add = T) #Plot on top of woody cover raster

Woody.crop <- crop(Woody.Rast, Deer.bbox) #Crop Woody Raster to remove unneeded data and to make processing time quicker

plot(Woody.crop)  #Plot

Woody.PCover <- raster::aggregate(Woody.crop, fact = 10, fun = mean) #Change the resolution to 15mx15m, while calculating the mean for each cell to get percent woody cover

plot(Woody.PCover) #Plot
Woody.PCover #Check Resolution and CRS again

writeRaster(Woody.PCover, "./FinalAnalysisData/PercentWoodyCover.tif")

rm(Deer.bbox)
rm(Woody.crop)
rm(Woody.Rast)

######################Soil Data Raster Prep############################

