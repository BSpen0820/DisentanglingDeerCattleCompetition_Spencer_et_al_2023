#Deer Movement in Responce to Stocking Event
#Bryan Spencer
#04/10/2023

##################Load Libraries##################

library(sf)
library(tidyverse)
library(amt)
library(lme4)
library(lmerTest)
library(performance)
library(ggplot2)
library(ciTools)

###################################################

all.df <- read.csv("./FinalAnalysisOutput/Model Data/movementdata.csv", header = T) #load in movement data

#log transform the velocity data so residuals better fit a normal distribution
all.df$logVel <- log(all.df$vel) 

all.df <- all.df[!is.infinite(all.df$logVel),] #remove any infinite values

all.df$StockingRaster <- all.df$StockingRaster*100 #convert stocking density from AU/ha to AU/km^2

vel.model <- lmer(logVel ~ StockingRaster + (1|ID), data = all.df) #Velocity Model

summary(vel.model)
check_model(vel.model)

confint(vel.model, level = 0.85)

################GGplot############################################

new.data <- data.frame(matrix(nrow = 100, ncol = 1))
names(new.data) <- "StockingRaster"

new.data$StockingRaster <- seq(from = min(all.df$StockingRaster, na.rm = T), to = max(all.df$StockingRaster, na.rm = T), length = 100)
new.data <- add_ci(new.data, vel.model, alpha = .15, includeRanef = F, nsims = 50000, type = "boot")

new.data <- new.data %>% mutate_at(vars(2:4), function(x){(exp(x))*60*60})

plot <- ggplot(new.data) +
  geom_line(aes(x = StockingRaster, y = pred)) +
  geom_ribbon(aes(x = StockingRaster, ymin = LCB0.075, ymax = UCB0.925), alpha = .5) +
  ylab("Movement Speed (m/hr)") + 
  xlab(bquote(bold("Stocking Density"~(AU/km^2)))) + 
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black", fill = "transparent"),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 19)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title = element_text(size = 18, face = "bold"))


