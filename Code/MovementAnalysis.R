#Deer Movement in Responce to Stocking Event
#Bryan Spencer
#01/31/2022

##################Load Libraries##################

library(sf)
library(tidyverse)
library(amt)
library(lme4)
library(lmerTest)

#################Data Prep#######################

#read in Deer GPS data
Deer <- tibble(read.csv("./FinalAnalysisData/DeerGPSData.csv", header = T))

#Assign the POSTime column as the class POSIXct
Deer$POSTime <- as.POSIXct(Deer$POSTime, tz = "America/Chicago", format = "%Y-%m-%d %H:%M:%OS")

#Remove NAs
Deer <- Deer %>% filter(!is.na(LONGITUDE))

#Change CRS of deer GPS data to UTM
Deer <- st_as_sf(Deer, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = F)
Deer <- st_transform(Deer, crs = 26914)
Deer <- cbind(Deer, data.frame(st_coordinates(Deer)))

#For Loop preparation for velocity and turn angle calculations
Ind <- unique(Deer$ID) #so it can cycle through each deer
Case <- unique(Deer$StStatus) #so it can cycle through before and after stocking event

ind.lst <- list() #empty individual list
all.lst <- list() #empty all list

for(i in 1:16) {
  #i <- 1 #debugger
  
  ind <- Deer %>% filter(ID == Ind[i]) #filter to one individual
  
  for (x in 1:2){
    #x <- 1 #debugger
    
    ind.cs <- ind %>% filter(StStatus == Case[x]) #filter to either before or after
    
    #create a track
    ind.cs.tr <- mk_track(ind.cs, .x = X, .y = Y, .t = POSTime, id = ID, crs = 26914) %>%                                           track_resample(rate = hours(3), tolerance = minutes(10))
    
    #Calculate velocity in m/s
    ind.vel <- speed(ind.cs.tr, append_na = F)
    
    #create steps so you can pull the turn angle
    ind.steps <- steps(ind.cs.tr)
    ind.ta <- ind.steps$ta_
    
    #create a df to combine all the movement information and add the individual id and stocking case
    df <- data.frame(ind.vel, ind.ta)
    df$ID <- Ind[i]
    df$StCase <- Case[x]
    
    #Save to individual movement data list
    ind.lst[[x]] <- df
  }
  
  #create an empty dataframe to merge the before and after movement data into
  ind.df <- data.frame(matrix(nrow = 0, ncol = ncol(df)))
  names(ind.df) <- names(df)
  
  for (k in 1:2){
    ind.df <- rbind(ind.df, ind.lst[[k]])
  
  }
  
  #save individual movement data to an all list
  all.lst[[i]] <- ind.df
  
}

#merge all the data frames in the movement list
all.df <- data.frame(matrix(nrow = 0, ncol = ncol(df)))
names(all.df) <- names(df)

for (k in 1:16){
  all.df <- rbind(all.df, all.lst[[k]])
  
}    

#Save the dataframe containing every individuals movement data
write.csv(all.df, "./FinalAnalysisOutput/movementdata.csv", row.names = F)

###################Movement Analysis##########################

load("./FinalAnalysisOutput/MovementModels.RData") #load in models to save time

all.df <- read.csv("./FinalAnalysisOutput/movementdata.csv", header = T) #load in movement data
all.df$StCase <- factor(all.df$StCase) #factor the stocking case

#log transform the velocity data so that is has a "normal" distribution
all.df$logVel <- log(all.df$ind.vel) 

all.df <- all.df[!is.infinite(all.df$logVel),] #remove any infinite values

vel.model <- lmer(logVel ~ StCase + (1|ID), data = all.df) #Velocity Model
summary(vel.model)

ta.model <- lmer(ind.ta ~ StCase + (1|ID), data = all.df) #Turn Angle Model
summary(ta.model)

#save models so they don't need to be re-run
save(list = c("vel.model", "ta.model"), file = "./FinalAnalysisOutput/MovementModels.RData")
