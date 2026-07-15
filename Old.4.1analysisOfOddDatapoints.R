### install and load packages
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("ggrastr")

library(dplyr)
library(ggplot2)
library(ggrastr)

#load in variables
# # Windows
# strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/dataWorking"
# strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/dataWorking"

# Linux
strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/dataWorking"
strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/dataWorking"


index = 0

### Load Working Data frames
strLoadFilename <- "lowDepth.csv"
strFullLoadName <- paste0(strInPath,"/",strLoadFilename)
dfLowDepth <- read.csv(strFullLoadName)

strLoadFilename <- "lowDepthHighPressure.csv"
strFullLoadName <- paste0(strInPath,"/",strLoadFilename)
dfLowDepthHighPressure <- read.csv(strFullLoadName)

strLoadFilename <- "lowPressure.csv"
strFullLoadName <- paste0(strInPath,"/",strLoadFilename)
dfLowPressure <- read.csv(strFullLoadName)

strLoadFilename <- "goodValues.csv"
strFullLoadName <- paste0(strInPath,"/",strLoadFilename)
dfNonProblemValues <- read.csv(strFullLoadName)
# 
# dfHighPressureLowDepth$sensorid <- as.character(dfHighPressureLowDepth$sensorid)
# dfRemovedValues <- union(union(dfLowDepth,dfLowPressure), dfHighPressureLowDepth)


### Various analyses
#dfAnalyte <- union(dfAnalyte2, dfNonProblemValues)
#dfAnalyte <- union(dfAnalyte1, dfAnalyte2)
dfAnalyte <- dfNonProblemValues
#dfAnalyte <- dfNonProblemValues[dfNonProblemValues$projectid=="EMPA"&dfNonProblemValues$region=="North",]
unique(dfAnalyte$region)
unique(dfAnalyte$projectid)
unique(dfAnalyte$estuaryname)
unique(dfAnalyte$year)
unique(dfAnalyte$season)
unique(dfAnalyte$profile)
unique(dfAnalyte$wqnotes)
unique(dfAnalyte$sensortype)
unique(dfAnalyte$sensorlocation)
unique(dfAnalyte$raw_pressure_unit)

# sum(dfLowPressure$wqnotes == "Specific Conductivity only. ODO %Saturation.",na.rm=TRUE)

ggplot(dfAnalyte, aes(x = rawPressureCm, y = rawWaterDepthMeters)) +
  geom_bin2d(bins = 200) +
#  geom_abline(intercept = coef(lmDepth2)[1],slope = coef(lmDepth2)[2],color = "red",linewidth = 1) +
  labs(title = "Depth Vs Pressure") +
  theme_minimal()

dfAnalyte <- dfAnalyte[dfAnalyte$rawPressureCm < 750,]
dfAnalyte1 <- dfAnalyte[dfAnalyte$raw_pressure_unit == "mbar",]
dfAnalyte2 <- dfAnalyte[dfAnalyte$raw_pressure_unit == "psi",]
dfAnalyte3 <- dfAnalyte[dfAnalyte$raw_pressure_unit == "cmH2O",]


#Model Comparison
lmModelOne<-lm(rawWaterDepthMeters~rawPressureCm, data=dfNonProblemValues)
print(summary(lmModelOne), digits = 10)


lmModelTwo<-lm(rawWaterDepthMeters~rawPressureCm, data=dfAnalyte)
summary(lmModelTwo)


lmModelThree<-lm(rawWaterDepthMeters~rawPressureCm, data=dfAnalyte3)
summary(lmModelThree)


### Seperating and analizing low depth section
dfAnalyte <- dfLowDepth
dfAnalyte <- dfAnalyte[dfAnalyte$rawPressureCm<500,]



### Garbage Collector
gc()
