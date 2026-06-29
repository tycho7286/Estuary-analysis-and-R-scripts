### install and load packages
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("ggrastr")
# install.packages("patchwork")

library(dplyr)
library(ggplot2)
library(ggrastr)
library(patchwork)

### Load in variables and read datafile
# # Windows
# strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
# strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"

# Linux
strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"

strLoadFilename <- "datasetWorkingCopy.csv"
strWriteFilename <- "workingDrakes.csv"
strFullLoadName <- paste0(strInPath,"/",strLoadFilename)
index = 0
depthVsPressureIntercept <- 3.837748327e-04 #Copied from previous Model Depth in meters pressure in cmH2O
depthVsPressureSlope <- 9.881125021e-03 #Copied from previous Model Depth in meters pressure in cmH2O
#estuaryCombined <- read.csv(strFullLoadName)
estuaryList <- sort(unique(estuaryCombined$estuaryname))
estuaryName <- "Drakes Estero"
# list of full moon dates 2024:
dfFullMoons2024 <- data.frame(DateTime = as.POSIXct(c("2024-01-25","2024-02-24","2024-03-25","2024-04-23","2024-05-23","2024-06-21","2024-07-21","2024-08-19","2024-09-17","2024-10-17","2024-11-15","2024-12-15"), tz = "UTC"))

# 
# ### Set up Time Columns ### This should be added to the cleaning or calculation step
# estuaryCombined$DateTime <- as.POSIXct(estuaryCombined$time,format = "%Y-%m-%dT%H:%M:%SZ",tz = "UTC") ### This should be added to the cleaning or calculation step
# 

### Filter for specific estuary, sensor depth, pressure reading, 
dfEstuary <- estuaryCombined[estuaryCombined$estuaryname==estuaryName,]
unique(dfEstuary$profile)
dfDeep <- dfEstuary[dfEstuary$profile=="bottom",]
dfDeep <- dfDeep[!is.na(dfDeep$raw_pressure) &dfDeep$raw_pressure != "NA",]
unique(dfDeep$raw_pressure)
nrow(dfDeep[dfDeep$raw_pressure=="NA",])
#   
# 
# ### Pressure to depth
# unique(dfDeep$raw_pressure_unit)
# dfDeep$rawWaterDepthMeters <- dfDeep$raw_pressure*depthVsPressureSlope+depthVsPressureIntercept
# dfDeep["rawWaterDepthMeters_units"] <- "m"
# 


### Subset by station
unique(dfDeep$stationno)
dfDrakes13 <- dfDeep[dfDeep$stationno==13,]
dfDrakes3 <- dfDeep[dfDeep$stationno==3,]
dfDrakes23 <- dfDeep[dfDeep$stationno==23,]
dfDrakes2 <- dfDeep[dfDeep$stationno==2,]

nrow(dfDeep)
nrow(dfDrakes13)+nrow(dfDrakes3)+nrow(dfDrakes23)+nrow(dfDrakes2)

### List years of operation for each subset
range(format(dfDrakes13$DateTime, "%Y"))
range(format(dfDrakes2$DateTime, "%Y"))
range(format(dfDrakes23$DateTime, "%Y"))
range(format(dfDrakes3$DateTime, "%Y"))











### Since only Station 2 was active for 2024-2025 do that one
strStationNo <- "2"
dfDrakesDepth <- dfDrakes2
plot1<-ggplot(dfDrakesDepth, aes(x = DateTime, y = calculatedWaterDepthMeters )) +
  geom_line() +
  labs(
    title = paste0(estuaryName," Water Depth Time Series for 2024 to 2025"),
    x = "Date",
    y = "Calculated Water Depth (m)"
  ) +
  theme_minimal()
plot1

### Get the Temperature, Salinity, and O2 data and plot them as well
#Temp
dfDrakesTemp <- dfEstuary[dfEstuary$stationno == strStationNo,]
dfDrakesTemp <- dfDrakesTemp[dfDrakesTemp$sensortype=="CTD"&dfDrakesTemp$profile=="bottom",]
plot2<-ggplot(dfDrakesTemp, aes(x = DateTime, y = raw_h2otemp)) +
  geom_line() +
  labs(
    title = paste0(estuaryName," Water Temp Time Series for 2024 to 2025"),
    x = "Date",
    y = "Water Temperature (deg C)"
  ) +
  theme_minimal()
plot2

#Salinity as measured by conductivity
dfDrakesSal <- dfEstuary[dfEstuary$stationno == "2"&!is.na(dfEstuary$calculatedSalPSU),]
dfDrakesSal <- dfDrakesSal[dfDrakesSal$profile=="bottom",]
plot3<-ggplot(dfDrakesSal, aes(x = DateTime, y = raw_conductivity)) +
  geom_line() +
  labs(
    title = paste0(estuaryName,"Calculated Salinity Time Series for 2024 to 2025"),
    x = "Date",
    y = "Calculated Salinity (PPT)"
  ) +
  theme_minimal()
plot3

#Direct O2 Sensor Measurement
dfDrakesO2 <- dfEstuary[dfEstuary$stationno == "2"&!is.na(dfEstuary$calculatedDOPct),]
dfDrakesO2 <- dfDrakesO2[(dfDrakesO2$sensorid!="791868"),]
plot4<-ggplot(dfDrakesO2, aes(x = DateTime, y = raw_do)) +
  geom_line() +
  labs(
    title = paste0(estuaryName," O2 Level Time Series for 2024 to 2025"),
    x = "Date",
    y = "Dissolved O2 Percentage (%)"
  ) +
  theme_minimal()
unique(dfDrakesO2$raw_do_unit)
plot4

#plot all 4 graphs 1 above the other
plot1/plot2/plot3/plot4










#check for duplication
sum(duplicated(dfDrakesDepth$DateTime))
sum(duplicated(dfDrakesTemp$DateTime))
sum(duplicated(dfDrakesSal$DateTime))
sum(duplicated(dfDrakesO2$DateTime))


# dupTemp <- dfDrakesTemp[
#   duplicated(dfDrakesTemp$DateTime) |
#   duplicated(dfDrakesTemp$DateTime, fromLast = TRUE),
# ]
# dupTemp <- dupTemp[order(dupTemp$DateTime),]
# unique(dupTemp$sensorid)
# 
# dupSal <- dfDrakesSal[
#   duplicated(dfDrakesSal$DateTime) |
#   duplicated(dfDrakesSal$DateTime, fromLast = TRUE),
# ]
# dupSal <- dupSal[order(dupSal$DateTime),]
# unique(dupSal$sensortype)
# unique(dupSal$profile)
# 
# dupO2<-dfDrakesO2[
#   duplicated(dfDrakesO2$DateTime) |
#   duplicated(dfDrakesO2$DateTime, fromLast = TRUE),
# ]
# dupO2 <- dupO2[order(dupO2$DateTime),]
# unique(dupO2$sensorid)
# sum(dupO2$sensorid=="791868")
# sum(dupO2$sensorid=="317269")
# sum(dfDrakesO2$sensorid=="317269")
# sum(dfDrakesO2$sensorid=="791868")
# 






### Set up time windows and plots
#Window One
timeStartWindowOne<-as.POSIXct("2024-11-01", tz = "UTC")
timeEndWindowOne<-as.POSIXct("2024-12-01", tz = "UTC")

# Depth
dfDrakesDepthWindowOne <- dfDrakesDepth[dfDrakesDepth$DateTime >= timeStartWindowOne & dfDrakesDepth$DateTime <  timeEndWindowOne,]
plot1<-ggplot(dfDrakesDepthWindowOne, aes(x = DateTime, y = calculatedWaterDepthMeters)) +
  geom_line() +
  labs(
    title = paste0(estuaryName," Water Depth Between ",format(timeStartWindowOne,"%B %Y")," and ",format(timeEndWindowOne, "%B %Y")),
    x = "Date",
    y = "Calculated Water Depth (m)"
  ) +
  theme_minimal()
plot1

# Temperature
dfDrakesTempWindowOne <- dfDrakesTemp[dfDrakesTemp$DateTime >= timeStartWindowOne & dfDrakesTemp$DateTime <  timeEndWindowOne,]
plot2<-ggplot(dfDrakesTempWindowOne, aes(x = DateTime, y = raw_h2otemp)) +
  geom_line() +
  labs(
    title = paste0(estuaryName," Water Temp Between ",format(timeStartWindowOne,"%B %Y")," and ",format(timeEndWindowOne, "%B %Y")),
    x = "Date",
    y = "Water Temp (deg C))"
  ) +
  theme_minimal()
plot2

# Salinity
dfDrakesSalWindowOne <- dfDrakesSal[dfDrakesSal$DateTime >= timeStartWindowOne & dfDrakesSal$DateTime <  timeEndWindowOne,]
plot3<-ggplot(dfDrakesSalWindowOne, aes(x = DateTime, y = calculatedSalPSU)) +
  geom_line() +
  labs(
    title = paste0(estuaryName," Calculated Salinty Between ",format(timeStartWindowOne,"%B %Y")," and ",format(timeEndWindowOne, "%B %Y")),
    x = "Date",
    y = "Calculated Salinity (PPT)"
  ) +
  theme_minimal()
plot3

#O2
dfDrakesO2WindowOne <- dfDrakesO2[dfDrakesO2$DateTime >= timeStartWindowOne & dfDrakesO2$DateTime <  timeEndWindowOne,]
dfDrakesO2WindowOne$calculatedDOPct <- as.numeric(dfDrakesO2WindowOne$calculatedDOPct)
plot4<-ggplot(dfDrakesO2WindowOne, aes(x = DateTime, y = calculatedDOPct)) +
  geom_line() +
  labs(
    title = paste0(estuaryName," Dissolved O2 Between ",format(timeStartWindowOne,"%B %Y")," and ",format(timeEndWindowOne, "%B %Y")),
    x = "Date",
    y = "Dissolved O2 Percentage (%)" 
  ) +
  theme_minimal()
plot4

#plot all 4 graphs 1 above the other
plot1/plot2/plot3/plot4



 



# Old Code before 2026-06-09
# 
# #look at the odd depth readings from jan to march 2005
# timeStartWindow<-as.POSIXct("2025-01-19", tz = "UTC")
# timeEndWindow<-as.POSIXct("2025-03-11", tz = "UTC")
# dfDrakesOddJan <- dfDrakes2[dfDrakes2$DateTime >= timeStartWindow &dfDrakes2$DateTime <  timeEndWindow,]
# ggplot(dfDrakesOddJan, aes(x = DateTime, y = rawWaterDepthMeters)) +
#   geom_line() +
#   labs(
#     title = "Odd Depth readings in early 2025 at Drakes",
#     x = "Date",
#     y = "Water Depth (m)"
#   ) +
#   theme_minimal()
# unique(dfDrakesOddJan$raw_pressure)
# 
# timeStartWindow<-as.POSIXct("2025-06-06", tz = "UTC")
# timeEndWindow<-as.POSIXct("3000-01-01", tz = "UTC")
# dfDrakesOddJan <- dfDrakes2[dfDrakes2$DateTime >= timeStartWindow &dfDrakes2$DateTime <  timeEndWindow,]
# 
# ggplot(dfDrakesOddJune, aes(x = DateTime, y = rawWaterDepthMeters)) +
#   geom_line() +
#   labs(
#     title = "Odd Depth readings in late 2025 at Drakes",
#     x = "Date",
#     y = "Water Depth (m)"
#   ) +
#   theme_minimal()
# unique(dfDrakesOddJan$raw_pressure)
# 
# 
# ### Because of odd readings from Jan to March in 2005 and very odd readings after July 2005, only look at 2004 data
# timeStartWindow<-as.POSIXct("2024-01-01", tz = "UTC")
# timeEndWindow<-as.POSIXct("2025-01-14", tz = "UTC")
# dfDrakes2_2024 <- dfDrakes2[
#   dfDrakes2$DateTime >= timeStartWindow &
#   dfDrakes2$DateTime <  timeEndWindow,
# ]
# 
# 
# ### find max depth and make a 60 day window
# maxWaterRow <- which.max(dfDrakes2_2024$rawWaterDepthMeters)
# maxWaterTime <- dfDrakes2_2024$DateTime[maxWaterRow]
# timePeakWindowStart <- (maxWaterTime - 30 * 24 * 60 * 60) 
# timePeakWindowEnd <- (maxWaterTime + 30 * 24 * 60 * 60)
# 
# dfPeakWindowDepth <- dfDrakes2_2024[
#   dfDrakes2_2024$DateTime >= timePeakWindowStart &
#   dfDrakes2_2024$DateTime <= timePeakWindowEnd,
# ]
# 
# dfFullMoonsInWindow <- dfFullMoons2024[dfFullMoons2024$DateTime >= timePeakWindowStart & dfFullMoons2024$DateTime <= timePeakWindowEnd,, drop = FALSE]
# 
# ggplot(dfPeakWindowDepth, aes(x = DateTime, y = rawWaterDepthMeters)) +
#   geom_line() +
#   geom_vline(
#     data = dfFullMoonsInWindow,
#     aes(xintercept = DateTime),
#     color = "blue",
#     linetype = "dashed"
#   ) +
#   annotate(
#     "text",
#     x = dfFullMoonsInWindow$DateTime,
#     y = Inf,
#     label = "FM",
#     vjust = 1.5,
#     size = 3
#   )+
#   labs(
#     title = "Drakes Estero Water Depth 60 Day Window Near Peak 2024",
#     x = "Date",
#     y = "Water Depth (m)"
#   ) +
#   theme_minimal()
# 
# ### find max depth and make a 60 day window in spring/summer
# dfDrakes2_2024MarSept <- dfDrakes2[
#   format(dfDrakes2$DateTime, "%Y") == "2024" &
#     as.numeric(format(dfDrakes2$DateTime, "%m")) >= 3 &
#     as.numeric(format(dfDrakes2$DateTime, "%m")) <= 9,
# ]
# maxWaterSummerRow <- which.max(dfDrakes2_2024MarSept$rawWaterDepthMeters)
# maxWaterSummerTime <- dfDrakes2_2024MarSept$DateTime[maxWaterSummerRow]
# timeSummerPeakWindowStart <- (maxWaterSummerTime - 30 * 24 * 60 * 60) 
# timeSummerPeakWindowEnd <- (maxWaterSummerTime + 30 * 24 * 60 * 60)
# 
# dfSummerPeakWindowDepth <- dfDrakes2_2024MarSept[
#   dfDrakes2_2024MarSept$DateTime >= timeSummerPeakWindowStart &
#   dfDrakes2_2024MarSept$DateTime <= timeSummerPeakWindowEnd,
# ]
# 
# dfFullMoonsInWindow <- dfFullMoons2024[dfFullMoons2024$DateTime >= timeSummerPeakWindowStart & dfFullMoons2024$DateTime <= timeSummerPeakWindowEnd,, drop = FALSE]
# 
# 
# ggplot(dfSummerPeakWindowDepth, aes(x = DateTime, y = rawWaterDepthMeters)) +
#   geom_line() +
#   geom_vline(
#     data = dfFullMoonsInWindow,
#     aes(xintercept = DateTime),
#     color = "blue",
#     linetype = "dashed"
#   ) +
#   annotate(
#     "text",
#     x = dfFullMoonsInWindow$DateTime,
#     y = Inf,
#     label = "FM",
#     vjust = 1.5,
#     size = 3
#   )+
#   labs(
#     title = "Drakes Estero Water Depth 60 Day Window Near Spring/Summer Peak Depth 2024",
#     x = "Date",
#     y = "Water Depth (m)"
#   ) +
#   theme_minimal()







#plot all 4 graphs 1 above the other
plot1/plot2/plot3/plot4


#Garbage Collector
gc()
