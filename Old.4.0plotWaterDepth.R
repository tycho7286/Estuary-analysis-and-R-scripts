### install and load packages
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("ggrastr")

library(dplyr)
library(ggplot2)
library(ggrastr)

#load in variables and read datafile
# # Windows
# strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/dataWorking"
# strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/dataWorking"

# Linux
strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/dataWorking"
strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/dataWorking"

strReadFilename <- "datasetWorkingCopy.csv"
strWriteFilename <- "datasetWorkingCopy2.csv"
strFullName <- paste0(strInPath,"/",strReadFilename)
index = 0
estuaryCombined <- read.csv(strFullName)

### Variables for this session
estuaryList <- unique(estuaryCombined$estuaryname)
print(estuaryList)
estuaryName <- estuaryList[17]
print(estuaryName)

dfWorkingSubset <- estuaryCombined

#subset by corrected water depth
dfWorkingSubset <- estuaryCombined[estuaryCombined$rawWaterDepthMeters_units == "m"&!is.na(estuaryCombined$rawWaterDepthMeters_units),]

#further subset by region
# dfWorkingSubset <- dfWorkingSubset[dfWorkingSubset$region == "North"&!is.na(dfWorkingSubset$region),]
# workingEstuaryList <- unique(dfWorkingSubset$estuaryname)


### Time Stuff
# index = 0
# for(i in workingEstuaryList){
#   index <- index + 1
#   dfWorkingSubsetLoop<-dfWorkingSubset[dfWorkingSubset$estuaryname==i,]
#   dates_posix <- as.POSIXct(dfWorkingSubsetLoop$time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
#   print(paste0(index," ",i," ",range(dates_posix)))
# #  print(paste0(index," ",i))
#     rm(dfWorkingSubsetLoop)
# }

#subset by estuary name
# dfWorkingSubset <- estuaryCombined[estuaryCombined$estuaryname == estuaryName&!is.na(estuaryCombined$estuaryname),]
# print(unique(dfWorkingSubset$rawWaterDepthMeters_units))
# print(unique(dfWorkingSubset$rawPressureCm_unit))


### prepare model raw pressure to water depth
dfWorkingSubset <- dfWorkingSubset[dfWorkingSubset$rawWaterDepthMeters!="Not Recorded",]
dfWorkingSubset <- dfWorkingSubset[dfWorkingSubset$rawPressureCm_unit != "Not Recorded",]
print(unique(dfWorkingSubset$rawPressureCm_unit))
print(unique(dfWorkingSubset$rawWaterDepthMeters_units))
dfWorkingSubset$rawPressureCm <- as.numeric(dfWorkingSubset$rawPressureCm)
dfWorkingSubset$rawWaterDepthMeters <- as.numeric(dfWorkingSubset$rawWaterDepthMeters)
sum(dfWorkingSubset$rawWaterDepthMeters)
sum(dfWorkingSubset$rawPressureCm)

# Model and Plot
lmDepth<-lm(rawWaterDepthMeters~rawPressureCm, data=dfWorkingSubset)
ggplot(dfWorkingSubset, aes(x = rawPressureCm, y = rawWaterDepthMeters)) +
  geom_bin2d(bins = 200) +
  geom_abline(intercept = coef(lmDepth)[1], slope = coef(lmDepth)[2], color = "red", linewidth = 1) +
  labs(title = "Unmodified Water Depth vs Raw Pressure") +
  theme_minimal()
summary(lmDepth)

# 
# ### prepare model raw pressure to water depth after removing negative and 0 depths
# dfModelAboveZeroSubset <- dfWorkingSubset[dfWorkingSubset$rawWaterDepthMeters>0.11,]
# dfModelAboveZeroSubset <- dfModelAboveZeroSubset[dfModelAboveZeroSubset$rawPressureCm >3,]
# 
# #Model and Plot
# lmDepth2<-lm(rawWaterDepthMeters~rawPressureCm, data=dfModelAboveZeroSubset)
# ggplot(dfModelAboveZeroSubset, aes(x = rawPressureCm, y = rawWaterDepthMeters)) +
#   geom_bin2d(bins = 200) +
#   geom_abline(intercept = coef(lmDepth2)[1],slope = coef(lmDepth2)[2],color = "red",linewidth = 1) +
#   labs(title = "Depth Above 0.11 m Depth vs Raw Pressure Above 3 cmH2O") +
#   theme_minimal()
# summary(lmDepth2)


### using above zero subset, separate high pressure low depth values "Problem Values"
# 
# dfProblemValues <- dfModelAboveZeroSubset[dfModelAboveZeroSubset$rawWaterDepthMeters < 5
#                                           & dfModelAboveZeroSubset$rawPressureCm > 1000,]
# 
# dfNonProblemValues <- dfModelAboveZeroSubset[
#                                           !(dfModelAboveZeroSubset$rawWaterDepthMeters < 5
#                                           & dfModelAboveZeroSubset$rawPressureCm > 1000),]

### Isolate Low Depth Problematic Region

#create the low depth subset
dfLowDepth <- dfWorkingSubset[dfWorkingSubset$rawWaterDepthMeters<.25&dfWorkingSubset$rawPressureCm>500,]
ggplot(dfLowDepth, aes(x = rawPressureCm, y = rawWaterDepthMeters)) +
  geom_bin2d(bins = 200) +
#  geom_abline(intercept = coef(lmDepth)[1], slope = coef(lmDepth)[2], color = "red", linewidth = 1) +
  labs(title = "Low Depth Region Unmodified Water Depth vs Raw Pressure") +
  theme_minimal()

#remove low depth values from main dataset
dfNonProblemNoLowDepth <- dfWorkingSubset[dfWorkingSubset$rawWaterDepthMeters>=.25 | dfWorkingSubset$rawPressureCm<=500,]
lmNoVeryLowDepth<-lm(rawWaterDepthMeters~rawPressureCm, data=dfNonProblemNoLowDepth)
ggplot(dfNonProblemNoLowDepth, aes(x = rawPressureCm, y = rawWaterDepthMeters)) +
  geom_bin2d(bins = 200) +
  geom_abline(intercept = coef(lmNoVeryLowDepth)[1], slope = coef(lmNoVeryLowDepth)[2], color = "red", linewidth = 1) +
  labs(title = "Low Depth Removed Water Depth vs Raw Pressure") +
  theme_minimal()

#create the low depth high pressure subset
dfLowDepthHighPressure <- dfNonProblemNoLowDepth[dfNonProblemNoLowDepth$rawWaterDepthMeters<5&dfNonProblemNoLowDepth$rawPressureCm>500,]
lmLowDepthHighPressure<-lm(rawWaterDepthMeters~rawPressureCm, data=dfLowDepthHighPressure)
ggplot(dfLowDepthHighPressure, aes(x = rawPressureCm, y = rawWaterDepthMeters)) +
  geom_bin2d(bins = 200) +
  geom_abline(intercept = coef(lmLowDepthHighPressure)[1], slope = coef(lmLowDepthHighPressure)[2], color = "red", linewidth = 1) +
  labs(title = "Low Depth High Pressure Removed Water Depth vs Raw Pressure") +
  theme_minimal()
summary(lmLowDepthHighPressure)

#remove low depth high pressure values from main dataset
dfNonProblemNoLowDepth2 <- dfNonProblemNoLowDepth[dfNonProblemNoLowDepth$rawWaterDepthMeters>=5 | dfNonProblemNoLowDepth$rawPressureCm<=500,]
lmNoLowDepth<-lm(rawWaterDepthMeters~rawPressureCm, data=dfNonProblemNoLowDepth2)
ggplot(dfNonProblemNoLowDepth2, aes(x = rawPressureCm, y = rawWaterDepthMeters)) +
  geom_bin2d(bins = 200) +
  geom_abline(intercept = coef(lmNoLowDepth)[1], slope = coef(lmNoLowDepth)[2], color = "red", linewidth = 1) +
  labs(title = "Low Depth High Pressure Removed Water Depth vs Raw Pressure") +
  theme_minimal()
summary(lmNoLowDepth)

#create the low depth high pressure subset THESE VALUES ARE INTERMINGLED
dfNonProblemWorking2<-dfNonProblemNoLowDepth2
dfLowPressure <- dfNonProblemNoLowDepth2[dfNonProblemNoLowDepth2$rawPressureCm<5,]
ggplot(dfLowPressure, aes(x = rawPressureCm, y = rawWaterDepthMeters)) +
  geom_bin2d(bins = 200) +
  geom_abline(intercept = coef(lmDepth)[1], slope = coef(lmDepth)[2], color = "red", linewidth = 1) +
  labs(title = "Low Pressure Intermingled Water Depth vs Raw Pressure") +
  theme_minimal()

#remove low depth high pressure values from main dataset THIS THROWS AWAY THE BOTTOM OF THE "GOOD" LINE
dfNonProblemWorking3 <- dfNonProblemWorking2[dfNonProblemWorking2$rawPressureCm>=5,]
lmNonProblemWorking3<-lm(rawWaterDepthMeters~rawPressureCm, data=dfNonProblemWorking3)
ggplot(dfNonProblemWorking3, aes(x = rawPressureCm, y = rawWaterDepthMeters)) +
  geom_bin2d(bins = 200) +
  geom_abline(intercept = coef(lmNonProblemWorking3)[1], slope = coef(lmNonProblemWorking3)[2], color = "red", linewidth = 1) +
  labs(title = "Removed \"Problem Values\" Water Depth vs Raw Pressure") +
  theme_minimal()
summary(lmNonProblemWorking3)

#plot Non Problem and Low Depth High Pressure together
dfAnalyte <- union(dfNonProblemWorking3,dfLowDepthHighPressure)
ggplot(dfAnalyte, aes(x = rawPressureCm, y = rawWaterDepthMeters)) +
  geom_bin2d(bins = 200) +
  geom_abline(intercept = coef(lmNonProblemWorking3)[1], slope = coef(lmNonProblemWorking3)[2], color = "red", linewidth = 1) +
  geom_abline(intercept = coef(lmLowDepthHighPressure)[1], slope = coef(lmLowDepthHighPressure)[2], color = "green", linewidth = 1) +
  labs(title = "Low Depth High Pressure and \"Non Problem\" Values Together") +
  theme_minimal()



### Save Working Data frames
strWriteFilename <- "lowDepth.csv"
strFullWriteName <- paste0(strOutPath,"/",strWriteFilename)
write.csv(dfLowDepth, strFullWriteName, row.names = FALSE)


strWriteFilename <- "lowDepthHighPressure.csv"
strFullWriteName <- paste0(strOutPath,"/",strWriteFilename)
write.csv(dfLowDepthHighPressure, strFullWriteName, row.names = FALSE)


strWriteFilename <- "lowPressure.csv"
strFullWriteName <- paste0(strOutPath,"/",strWriteFilename)
write.csv(dfLowPressure, strFullWriteName, row.names = FALSE)


strWriteFilename <- "goodValues.csv"
strFullWriteName <- paste0(strOutPath,"/",strWriteFilename)
write.csv(dfNonProblemWorking3, strFullWriteName, row.names = FALSE)





### Model and Plot raw depth vs calculated depth comparison
dfDepthComparison <- estuaryCombined[!is.na(estuaryCombined$rawWaterDepthMeters),]

lmDepthComp<-lm(rawWaterDepthMeters~calculatedWaterDepthMeters, data=dfDepthComparison)
ggplot(dfDepthComparison, aes(x = calculatedWaterDepthMeters, y = rawWaterDepthMeters)) +
  geom_bin2d(bins = 200) +
  geom_abline(intercept = coef(lmDepthComp)[1], slope = coef(lmDepthComp)[2], color = "red", linewidth = 1) +
  labs(title = "Unmodified Water Depth vs Calculated Water Depth") +
  theme_minimal()
summary(lmDepthComp)



# Old Code
# 
# 
# # #Model and Plot Problem
# # lmDepthProblem<-lm(rawWaterDepthMeters~rawPressureCm, data=dfProblemValues)
# # ggplot(dfProblemValues, aes(x = rawPressureCm, y = rawWaterDepthMeters)) +
# #   geom_bin2d(bins = 200) +
# #   geom_abline(intercept = coef(lmDepthProblem)[1],slope = coef(lmDepthProblem)[2],color = "red",linewidth = 1) +
# #   labs(title = "Water Depth vs Raw Pressure of Low Depth High Pressure Values") +
# #   theme_minimal()
# # summary(lmDepthProblem)
# 
# #Model and Plot NonProblem
# lmDepthNonProblem<-lm(rawWaterDepthMeters~rawPressureCm, data=dfNonProblemValues)
# ggplot(dfNonProblemValues, aes(x = rawPressureCm, y = rawWaterDepthMeters)) +
#   geom_bin2d(bins = 200) +
#   geom_abline(intercept = coef(lmDepthNonProblem)[1],slope = coef(lmDepthNonProblem)[2],color = "red",linewidth = 1) +
#   labs(title = "Water Depth vs Raw Pressure After Removing Low Values and Low Depth High Pressure Region") +
#   theme_minimal()
# summary(lmDepthNonProblem)
# 
# # Prepare Data frames for low depth and low pressure data points
# dfLowDepth <- dfWorkingSubset[dfWorkingSubset$rawWaterDepthMeters<=0.11,]
# dfLowPressure <- dfWorkingSubset[dfWorkingSubset$rawPressureCm <=3,]
# 
# 
# #Model and Plot low values
# lmModelLowDepth<-lm(rawWaterDepthMeters~rawPressureCm, data=dfModelLowDepth)
# ggplot(dfModelLowDepth, aes(x = rawPressureCm, y = rawWaterDepthMeters)) +
#   geom_bin2d(bins = 200) +
#   geom_abline(intercept = coef(lmModelLowDepth)[1],slope = coef(lmModelLowDepth)[2],color = "red",linewidth = 1) +
#   labs(title = "Removed Low Depth Values") +
#   theme_minimal()
# summary(lmModelLowDepth)
# 
# lmModelLowPressure<-lm(rawWaterDepthMeters~rawPressureCm, data=dfLowPressure)
# ggplot(dfLowPressure, aes(x = rawPressureCm, y = rawWaterDepthMeters)) +
#   geom_bin2d(bins = 200) +
#   geom_abline(intercept = coef(lmModelLowPressure)[1],slope = coef(lmModelLowPressure)[2],color = "red",linewidth = 1) +
#   labs(title = "Removed Low Pressure Values") +
#   theme_minimal()
# summary(lmModelLowPressure)
# 
# dfRemovedValues <- union(union(dfModelLowDepth,dfRemovedValues),dfProblemValues)
# lmRemovedValues<-lm(rawWaterDepthMeters~rawPressureCm, data=dfRemovedValues)
# ggplot(dfRemovedValues, aes(x = rawPressureCm, y = rawWaterDepthMeters)) +
#   geom_bin2d(bins = 200) +
#   geom_abline(intercept = coef(lmRemovedValues)[1],slope = coef(lmRemovedValues)[2],color = "red",linewidth = 1) +
#   labs(title = "All Removed Values") +
#   theme_minimal()
# summary(lmRemovedValues)
# 
# 
# 
# #Check Data
# dfModelLowValues<-union(dfModelLowDepth,dfLowPressure)
# nrow(dfNonProblemValues)
# nrow(dfProblemValues)
# nrow(dfModelLowDepth)
# nrow(dfLowPressure)
# nrow(dfNonProblemValues)+nrow(dfProblemValues)+nrow(dfModelLowValues)
# nrow(dfWorkingSubset)
# 
# 
# ### Save Working Data frames
# strWriteFilename <- "lowDepth.csv"
# strFullWriteName <- paste0(strOutPath,"/",strWriteFilename)
# write.csv(dfModelLowDepth, strFullWriteName, row.names = FALSE)
# 
# 
# strWriteFilename <- "lowPressure.csv"
# strFullWriteName <- paste0(strOutPath,"/",strWriteFilename)
# write.csv(dfLowPressure, strFullWriteName, row.names = FALSE)
# 
# 
# strWriteFilename <- "problemValues.csv"
# strFullWriteName <- paste0(strOutPath,"/",strWriteFilename)
# write.csv(dfProblemValues, strFullWriteName, row.names = FALSE)
# 
# 
# strWriteFilename <- "goodValues.csv"
# strFullWriteName <- paste0(strOutPath,"/",strWriteFilename)
# write.csv(dfNonProblemValues, strFullWriteName, row.names = FALSE)



### Garbage Collector
gc()

