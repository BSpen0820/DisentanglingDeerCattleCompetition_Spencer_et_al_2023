#Title: Home Range Analysis Presence vs Absence
#By: Bryan Spencer
#Date: 04/10/2023

#######Read in Libraries###################

library(tidyverse)
library(raster)
library(sf)
library(sp)
library(move)
library(lme4)
library(lmerTest)
library(performance)

###########################################\

# Read in Data
HR.table <- read.csv("./FinalAnalysisOutput/HRData.csv", header = T)

# Convert stocking density from AU/ha to AU/km^2
HR.table$Rate <- HR.table$StDens * 100

HR.model <- lmer(Area ~ StDens + (1|ID), data = HR.table)

summary(HR.model)
check_model(HR.model)

confint(HR.model, level = .85)

####################Boxplot###############################

hrplot <-
  ggplot(HR.table) +
  geom_boxplot(aes(x= Status, y= Area))+
  xlab("Stocking Event")+
  ylab("Area (ha)")+
  theme(axis.text = element_text(size = 6)) +
  theme(axis.title = element_text(size = 8, face = "bold")) +
  theme(panel.background = element_rect(fill = "transparent"),
panel.border = element_rect(color = "black", fill = "transparent"),
axis.title.x = element_text(margin = margin(t = 10)),
axis.title.y = element_text(margin = margin(r = 10)))
