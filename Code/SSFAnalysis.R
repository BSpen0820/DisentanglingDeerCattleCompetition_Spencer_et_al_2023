#Resource Selection Analysis
#Bryan Spencer
#1/31/2022

######################Load Libraries##############

library(tidyverse)
library(move)
library(amt)
library(raster)
library(sf)
library(lubridate)
library(survival)
library(AICcmodavg)

set.seed(4432)

#####################Landscape Co-variates##############

PerWoody <- raster("./FinalAnalysisData/PercentWoodyCover.tif")
PerSand <- raster("./FinalAnalysisData/PercentSand.tif")
DistWater <- raster("./FinalAnalysisData/Dist2Water.tif")
DistRoad <- raster("./FinalAnalysisData/Dist2Road.tif")
StockRast <- raster("./FinalAnalysisData/StockingRaster.tif")

landstack <- raster::stack(PerWoody, PerSand, DistWater, DistRoad, StockRast) #create a raster stack of co-variates

rm(list = c("PerWoody", "PerSand", "DistWater", "DistRoad", "StockRast")) #remove memory consuming rasters

#####################Step-Selection Prep###############

#read in Deer GPS data
Deer <- tibble(read.csv("./Output/GPS Data Prep/AllGPSAndStockingDens.csv", header = T))

#Assign the POSTime column as the class POSIXct
Deer$POSTime <- as.POSIXct(Deer$POSTime, tz = "America/Chicago", format = "%Y-%m-%d %H:%M:%OS")

#Remove NAs
Deer <- Deer %>% filter(!is.na(LONGITUDE))

#Change CRS of deer GPS data to that of the rasters
Deer <- st_as_sf(Deer, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = F)
Deer <- st_transform(Deer, crs = 26914)
Deer <- cbind(Deer, data.frame(st_coordinates(Deer)))

#For Loop preparation for random steps
Ind <- unique(Deer$ID) #so it can cycle through each deer
Case <- unique(Deer$StStatus) #so it can cycle through before and after stocking event

ssf.ind <- list() #empty individual ssf data list
ssf.all <- list() #empty all ssf data list

#For loop to cycle through each deer
for (i in 1:length(Ind)){
  
  ind <- Deer %>% filter(ID == Ind[i]) #filter to one individual
  
  #For loop to cycle through stocking status case
  for (x in 1:2){
    
    ind.cs <- ind %>% filter(StStatus == Case[x]) #filter to either before or after
    
    #create a track
    ind.cs.tr <- mk_track(ind.cs, .x = X, .y = Y, .t = POSTime, id = ID, crs = 26914) %>%          track_resample(rate = hours(3), tolerance = minutes(10))
    
    
    #create burst from which you can create your random steps and extract landscape covariates
    ssf <- ind.cs.tr %>% steps_by_burst() %>% random_steps(n_control = 15) %>% extract_covariates(landstack)
    
    ssf$id <- unique(ind.cs.tr$id) #add animal id to ssf data dataframe
    ssf$step_id <- paste(ssf$id, ".", ssf$t1_) #create a unique stepid for each step and possible step
    ssf$use <- as.numeric(ssf$case_) #make use vs available numeric
    ssf$Stocked <- Case[x] #copy the stocking status
    
    ssf.ind[[x]] <- ssf #save to ind ssf data list
    
  }
  
  #create an empty dataframe to merge the before and after ssf data into
  ssf.ind.df <- data.frame(matrix(nrow = 0, ncol = ncol(ssf)))
  names(ssf.ind.df) <- names(ssf)
  
  for (k in 1:2){
    ssf.ind.df <- rbind(ssf.ind.df, ssf.ind[[k]])
  }
  
  ssf.all[[i]] <- ssf.ind.df #save the merged individual ssf data to the all ssf data list
  
}

#create an empty dataframe to merge all individual ssf data into 
ssf.all.df <- data.frame(matrix(nrow = 0, ncol = ncol(ssf)))
names(ssf.all.df) <- names(ssf)

for (m in 1:19){
  ssf.all.df <- rbind(ssf.all.df, ssf.all[[m]])
}

rm(list = setdiff(ls(), "ssf.all.df")) #remove extra list and dataframes

#save the dataframe that contains all ssf data for all individuals
write.csv(ssf.all.df, "./FinalAnalysisOutput/ssfdata.csv", row.names = F) 

##############################Extract Stocking Data################

ssf.all.df <- read.csv("./FinalAnalysisOutput/ssfdata.csv", header = T) #read in ssf data

ssf.all.df$StockingRaster <- ifelse(ssf.all.df$Stocked == "before", 0, ssf.all.df$StockingRaster)

write.csv(ssf.all.df, "./FinalAnalysisOutput/ssfdata.csv", row.names = F)

#############################SSF Analysis##################

load("./FinalAnalysisOutput/SSFModels.RData") #will load in ssf model outputs below

ssf.all.df <- read.csv("./FinalAnalysisOutput/ssfdata.csv", header = T) #read in ssf data

#change case for before and after stocking event to 0 or 1
ssf.all.df$stcase <- ifelse(ssf.all.df$Stocked == "before", 0, 1)

ssf.all.df$Wood <- scale(ssf.all.df$PercentWoodyCover)
ssf.all.df$Sand <- scale(ssf.all.df$PercentSand)
ssf.all.df$Water <- scale(ssf.all.df$Dist2Water)
ssf.all.df$Road <- scale(ssf.all.df$Dist2Road)
ssf.all.df$StRate <- scale(ssf.all.df$StockingRaster)

#Modeled Analysis after Heather Abernathy Hurricane Paper
SSFModels <- list() #create an empty list to save all the models to

#Global Model
SSFModels[[1]] <- survival::clogit(data = ssf.all.df, formula = use ~ Wood + 
                                   Sand + Water + Road +
                                    StRate*(Wood + 
                                    Sand + Water + Road) + cluster(id) + 
                                    strata(step_id), method = 'approximate')

#Null Model
SSFModels[[2]] <- survival::clogit(data = ssf.all.df, formula = use ~ 1 + cluster(id) + 
                                  strata(step_id), method = 'approximate')

#Model without the interaction
SSFModels[[3]] <- survival::clogit(data = ssf.all.df, formula = use ~ PercentWoodyCover + 
                                  PercentSand + Dist2Water + Dist2Road + 
                                  StRate + cluster(id) + 
                                  strata(step_id), method = 'approximate')

#Model names
model.names <- c("global", "null", "minus-int")

#create data frame to display AIC scores
AICSSF <- aictab(cand.set = SSFModels, modnames = model.names, second.ord = F)

#Summary of best fitting model
GlobalSummary <- summary(SSFModels[[1]])

#Save so you don't have to run code above
save(list = c("AICSSF", "SSFModels", "GlobalSummary"), file = "./FinalAnalysisOutput/SSFModels.RData")

range(ssf.all.df$Wood)
