#Title: Spatial Variables Prep
#By: Bryan Spencer
#Date: 1/30/2022

##############Read in Libraries###########

library(tidyverse)
library(sf)
library(raster)
library(sp)

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

Woody.PCover <- raster("./FinalAnalysisData/PercentWoodyCover.tif")
Soil <- read_sf("./Data/Soil Data/spatial/soilmu_a_aoi.shp", layer = "soilmu_a_aoi")  #read in soil map

plot(st_geometry(Soil)) #plot

Soil <- st_transform(Soil, crs = crs(Woody.PCover)) #transform it to that of the woody raster
Soil #check crs

PrSand <- read.csv("./Data/Soil Data/thematic/rating7706522.csv", header = T) #Read in Percent Sand table for the soil type

PrSand <- PrSand[,c("MapUnitKey", "RatingNum")] #Remove all unnecessary information

Soil <- merge(Soil, PrSand, by.x = "MUKEY", by.y = "MapUnitKey", all.x = T, all.y = F) #match the soil classification to the percent sand

plot(Soil[5]) #plot the percent sand

EmptRaster <- raster::setValues(Woody.PCover, values = NA) #create an empty raster from the percent woody cover raster so cells line up

Soil <- as(Soil, Class = "Spatial") #convert soil data to a spatial object, required for rasterize function

SoilRaster <- rasterize(Soil, EmptRaster, field = "RatingNum") #rasterize percent sand data

plot(SoilRaster) #Plot

writeRaster(SoilRaster, "./FinalAnalysisData/PercentSand.tif", overwrite = T, bylayer = T, NAflag = 10000) #Save

####################Distance to Water Raster###################

water <- st_read("./Data/Water/watersources.shp") #read in water source points
plot(st_geometry(water)) #plot water sources

Woody.PCover <- raster("./FinalAnalysisData/PercentWoodyCover.tif") #read in woody percent raster
EmptRaster <- raster::setValues(Woody.PCover, values = NA) #make it an empty raster

Water.Dist <- distanceFromPoints(EmptRaster, water) #create a distance raster
plot(Water.Dist) #plot
plot(st_geometry(water), add = T) #plot water points

writeRaster(Water.Dist, "./FinalAnalysisData/Dist2Water.tif", overwrite = T) #save

rm(list = ls()) #clear enviroment

####################Distance to Road Raster###################

road <- st_read("./Data/Roads/Roads.shp") #Read in road shapefile
road$Class <- "Road" #add column that classifyies it all as roads
road$Class <- factor(road$Class) #make it a factor so it can be rasterized

empty.rast <- raster::setValues(raster("./FinalAnalysisData/PercentWoodyCover.tif"), values = NA) #create an epmty raster

road.rast <- rasterize(road, empty.rast, "Class") #Make a raster of just roads
plot(road.rast) #plot

road.points <- rasterToPoints(road.rast, spatial = T) #convert the road raster to poitns

road.dist <- distanceFromPoints(empty.rast, road.points) #create the distance raster from the road points

plot(road.dist) #Plot distance raster
plot(st_geometry(road), add = T) #add road shapefile to plot

writeRaster(road.dist, "./FinalAnalysisData/Dist2Road.tif", overwrite = T) #save
