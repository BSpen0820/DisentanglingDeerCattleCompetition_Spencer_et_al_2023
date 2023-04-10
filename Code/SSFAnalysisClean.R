#Resource Selection Analysis
#Bryan Spencer
#1/31/2022

######################Load Libraries##############

#some change

library(tidyverse)
library(move)
library(amt)
library(raster)
library(sf)
library(lubridate)
library(survival)
library(AICcmodavg)
library(car)

#############################SSF Analysis##################

ssf.all.df <- read.csv("./FinalAnalysisOutput/Model Data/ssfdata.csv", header = T) #read in ssf data

#change case for before and after stocking event to 0 or 1
ssf.all.df$stcase <- factor(ifelse(ssf.all.df$Stocked == "before", 0, 1))

ssf.all.df$PercentWoodyCover <- ssf.all.df$PercentWoodyCover*100 #change percent wood cover from proportion to actual percent
ssf.all.df$StockingRaster <- ssf.all.df$StockingRaster*100 #convert stocking density from AU/ha to AU/km^2

# Scale and center all covariates to make interpretation easier

ssf.all.df$Wood <- scale(ssf.all.df$PercentWoodyCover)
ssf.all.df$Sand <- scale(ssf.all.df$PercentSand)
ssf.all.df$Water <- scale(ssf.all.df$Dist2Water)
ssf.all.df$Road <- scale(ssf.all.df$Dist2Road)
ssf.all.df$StDens <- scale(ssf.all.df$StockingRaster)
ssf.all.df$StepL <- scale(ssf.all.df$sl_)
ssf.all.df$TurnA <- scale(ssf.all.df$ta_)


SSFModels <- list() #create an empty list to save all the models to

#Global Model
SSFModels[[1]] <- survival::clogit(data = ssf.all.df, formula = use ~ Wood + 
                                   Sand + Water + Road + StepL + TurnA +
                                    StDens*(Wood + 
                                    Sand + Water + Road) + cluster(id) + 
                                    strata(step_id), method = 'approximate', model = T)

#Null Model
SSFModels[[2]] <- survival::clogit(data = ssf.all.df, formula = use ~ 1 + StepL + TurnA + cluster(id) + 
                                  strata(step_id), method = 'approximate', model = T)

#Model without the interaction
SSFModels[[3]] <- survival::clogit(data = ssf.all.df, formula = use ~ Wood + 
                                  Sand + Water + Road + StepL + TurnA +
                                  StDens + cluster(id) + 
                                  strata(step_id), method = 'approximate', model = T)


#Model names
model.names <- c("global", "null", "minus-int")

#display AIC scores
aictab(cand.set = SSFModels, modnames = model.names, second.ord = F)

#Summary of best fitting model
summary(SSFModels[[1]])

#compute confidence intervals
confint.default(SSFModels[[1]], level = 0.85)

#Check co-linearity
vif(SSFModels[[1]])

###################Plotting Interactive effects in Model############################

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

mean.st <- ssf.all.df %>% filter(Stocked == "after") %>% summarise(mean = mean(StDens))

StDens <-  c(min(ssf.all.df$StDens), mean.st$mean[1], max(ssf.all.df$StDens))

dfWood <- data.frame(matrix(nrow = 300, ncol = 7))
names(dfWood) <- c("StDens", "Wood", "Sand", "Water", "Road", "StepL", "TurnA")

dfWood$Road <- 0
dfWood$Water <- 0
dfWood$Sand <- 0
dfWood$StepL <- 0
dfWood$TurnA <- 0
dfWood$Wood <- seq(from = min(ssf.all.df$Wood), to = max(ssf.all.df$Wood), length.out = 100)
dfWood$StDens <- rep(StDens, each = 100)
dfWood$step_id <- unique(ssf.all.df$step_id)[1]

df <- predict(SSFModels[[1]], dfWood, type = "risk", se.fit = T, reference = "sample")

dfWood$fit <- df$fit
dfWood$se.fit <- df$se.fit 
dfWood$prob <- (dfWood$fit)/(1 + dfWood$fit)
dfWood$up <- (dfWood$fit + dfWood$se.fit *1.44)/(1+(dfWood$fit + dfWood$se.fit *1.44))
dfWood$low <- (dfWood$fit - dfWood$se.fit *1.44)/(1+(dfWood$fit - dfWood$se.fit *1.44))

dfWood$PercWood <- dfWood$Wood * attr(ssf.all.df$Wood, 'scaled:scale') + attr(ssf.all.df$Wood, 'scaled:center')
dfWood$StDensCor <- dfWood$StDens * attr(ssf.all.df$StDens, 'scaled:scale') + attr(ssf.all.df$StDens, 'scaled:center')

WoodPlot <- ggplot() +
  geom_line(aes(x = PercWood, y = prob, color = factor(StDensCor)), lwd = .8, 
            data = dfWood) + 
  geom_ribbon(aes(x = PercWood, ymin = low, ymax = up, fill = factor(StDensCor)),
              data = dfWood, alpha = .2) +
  ylab("Probability of Use") + 
  xlab("Percent Brush (%)") + 
  scale_fill_manual(values = cbbPalette[6:8], name = bquote("Stocking Density"~(AU/km^2)), 
                    labels = c("0", "3.57", "15.6")) +
  scale_color_manual(values = cbbPalette[6:8], name = bquote("Stocking Density"~(AU/km^2)), 
                     labels = c("0", "3.57", "15.6")) +
  theme(legend.position = c(.75, .2),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black", fill = "transparent"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title = element_text(size = 18, face = "bold"))


dfSand <- data.frame(matrix(nrow = 300, ncol = 7))
names(dfSand) <- c("StDens", "Wood", "Sand", "Water", "Road", "StepL", "TurnA")

dfSand$StepL <- 0
dfSand$TurnA <- 0
dfSand$Road <- 0
dfSand$Water <- 0
dfSand$Sand <- seq(from = min(ssf.all.df$Sand), to = max(ssf.all.df$Sand), length.out = 100)
dfSand$Wood <- 0
dfSand$StDens <- rep(StDens, each = 100)
dfSand$step_id <- unique(ssf.all.df$step_id)[1]

df2 <- predict(SSFModels[[1]], dfSand, type = "risk", se.fit = T, reference = "sample")

dfSand$fit <- df2$fit
dfSand$se.fit <- df2$se.fit
dfSand$prob <- (dfSand$fit)/(1 + dfSand$fit)
dfSand$up <- (dfSand$fit + dfSand$se.fit *1.44)/(1+(dfSand$fit + dfSand$se.fit *1.44))
dfSand$low <- (dfSand$fit - dfSand$se.fit *1.44)/(1+(dfSand$fit - dfSand$se.fit *1.44))
dfSand$low <- ifelse(dfSand$low < 0, 0, dfSand$low)

dfSand$PercSand <- dfSand$Sand * attr(ssf.all.df$Sand, 'scaled:scale') + attr(ssf.all.df$Sand, 'scaled:center')
dfSand$StDensCor <- dfSand$StDens * attr(ssf.all.df$StDens, 'scaled:scale') + attr(ssf.all.df$StDens, 'scaled:center')

SandPlot <- ggplot() +
             geom_line(aes(x = PercSand, y = prob, color = factor(StDensCor)), lwd = .8, 
                       data = dfSand) + 
             geom_ribbon(aes(x = PercSand, ymin = low, ymax = up, fill = factor(StDensCor)),
                              data = dfSand, alpha = .2) +
            ylab("Probability of Use") + 
            xlab("Percent Sand (%)") + 
            scale_fill_manual(values = cbbPalette[6:8], name = bquote("Stocking Density"~(AU/km^2)), 
                              labels = c("0", "3.57", "15.6")) +
            scale_color_manual(values = cbbPalette[6:8], name = bquote("Stocking Density"~(AU/km^2)), 
                               labels = c("0", "3.57", "15.6")) + 
            theme(legend.position = c(.75, .2),
                  panel.background = element_rect(fill = "transparent"),
                  panel.border = element_rect(color = "black", fill = "transparent"),
                  legend.text = element_text(size = 10),
                  legend.title = element_text(size = 12)) +
            theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
            theme(plot.title = element_text(size = 20)) +
            theme(axis.text = element_text(size = 15)) +
            theme(axis.title = element_text(size = 18, face = "bold"))
