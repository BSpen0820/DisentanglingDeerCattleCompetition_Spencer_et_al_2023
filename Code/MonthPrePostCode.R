#Title: Deer Behavior Month Pre and Post Cattle Deployment
#Bryan Spencer


#Load Libraries
library(foreign)
library(tidyverse)

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