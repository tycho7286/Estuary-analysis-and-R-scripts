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
# strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/dataCombined"
# strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/dataWorking"

#Linux
strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/dataCombined"
strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/dataWorking"


strLoadFilename <- "combinedDataset.csv"
strWriteFilename <- "workingRussianRiver.csv"
strFullLoadName <- paste0(strInPath,"/",strLoadFilename)
index = 0
depthVsPressureIntercept <- 3.837748327e-04 #Copied from previous Model Depth in meters pressure in cmH2O
depthVsPressureSlope <- 9.881125021e-03 #Copied from previous Model Depth in meters pressure in cmH2O
#estuaryCombined <- read.csv(strFullLoadName)
estuaryList <- sort(unique(estuaryCombined$estuaryname))
# list of full moon dates 2024:
dfFullMoons2024 <- data.frame(DateTime = as.POSIXct(c("2024-01-25","2024-02-24","2024-03-25","2024-04-23","2024-05-23","2024-06-21","2024-07-21","2024-08-19","2024-09-17","2024-10-17","2024-11-15","2024-12-15"), tz = "UTC"))


### Set up Time Columns ### This should be added to the cleaning or calculation step
estuaryCombined$DateTime <- as.POSIXct(estuaryCombined$time,format = "%Y-%m-%dT%H:%M:%SZ",tz = "UTC") ### This should be added to the cleaning or calculation step


### Filter for specific estuary, sensor depth, pressure reading, 
dfEstuary <- estuaryCombined[estuaryCombined$estuaryname=="Russian River",]
unique(dfEstuary$profile)
dfDeep <- dfEstuary[dfEstuary$profile=="bottom",]
dfDeep <- dfDeep[!is.na(dfDeep$raw_pressure) &dfDeep$raw_pressure != "NA",]
unique(dfDeep$raw_pressure)
nrow(dfDeep[dfDeep$raw_pressure=="NA",])
  

### Pressure to depth
unique(dfDeep$raw_pressure_unit)
dfDeep$rawWaterDepthMeters <- dfDeep$raw_pressure*depthVsPressureSlope+depthVsPressureIntercept
dfDeep["rawWaterDepthMeters_units"] <- "m"



### Subset by station
unique(dfDeep$stationno)
# dfRus13 <- dfDeep[dfDeep$stationno==13,]
 dfRus3 <- dfDeep[dfDeep$stationno==3,]
# dfRus23 <- dfDeep[dfDeep$stationno==23,]
# dfRus2 <- dfDeep[dfDeep$stationno==2,]
# 
# nrow(dfDeep)
# nrow(dfRus13)+nrow(dfRus3)+nrow(dfRus23)+nrow(dfRus2)
# 
### List years of operation for each subset
# range(format(dfRus13$DateTime, "%Y"))
# range(format(dfRus2$DateTime, "%Y"))
# range(format(dfRus23$DateTime, "%Y"))
range(format(dfRus3$DateTime, "%B %Y"))











### Since only Station 3 was the only one active, use that
strStationNo <- "3"
dfRusDepth <- dfRus3
plot1<-ggplot(dfRusDepth, aes(x = DateTime, y = rawWaterDepthMeters)) +
  geom_line() +
  labs(
    title = "Russian River Water Depth Time Series for 2024 to 2025",
    x = "Date",
    y = "Water Depth (m)"
  ) +
  theme_minimal()
plot1

### Get the Temperature, Salinity, and O2 data and plot them as well
#Temp
dfRusTemp <- dfEstuary[dfEstuary$stationno == strStationNo,]
dfRusTemp <- dfRusTemp[dfRusTemp$sensortype=="CTD"&dfRusTemp$profile=="bottom",]
plot2<-ggplot(dfRusTemp, aes(x = DateTime, y = raw_h2otemp)) +
  geom_line() +
  labs(
    title = "Russian River Water Temp Time Series for 2024 to 2025",
    x = "Date",
    y = "Water Temperature (deg C)"
  ) +
  theme_minimal()
plot2

#Salinity as measured by conductivity
dfRusSal <- dfEstuary[dfEstuary$stationno == "3"&dfEstuary$raw_conductivity != "Not Recorded",]
dfRusSal <- dfRusSal[!is.na(dfRusSal$raw_conductivity) &dfRusSal$raw_conductivity != "NA",]
dfRusSal <- dfRusSal[dfRusSal$profile=="bottom",]
plot3<-ggplot(dfRusSal, aes(x = DateTime, y = raw_conductivity)) +
  geom_line() +
  labs(
    title = "Russian River Conductivity Time Series for 2024 to 2025",
    x = "Date",
    y = "Measured Conductivity (mS)"
  ) +
  theme_minimal()
plot3

#Direct O2 Sensor Measurement
dfRusO2 <- dfEstuary[dfEstuary$stationno == "3"&dfEstuary$raw_do != "Not Recorded",]
dfRusO2 <- dfRusO2[!is.na(dfRusO2$raw_do) &dfRusO2$raw_do != "NA",]
# dfRusO2 <- dfRusO2[(dfRusO2$sensorid!="791868"),]
plot4<-ggplot(dfRusO2, aes(x = DateTime, y = raw_do)) +
  geom_line() +
  labs(
    title = "Russian River O2 Level Time Series for 2024 to 2025",
    x = "Date",
    y = "Measured do (mg/L)"
  ) +
  theme_minimal()
unique(dfRusO2$raw_do_unit)
plot4

#plot all 4 graphs 1 above the other
plot1/plot2/plot3/plot4










#check for duplication
sum(duplicated(dfRusDepth$DateTime))
sum(duplicated(dfRusTemp$DateTime))
sum(duplicated(dfRusSal$DateTime))
sum(duplicated(dfRusO2$DateTime))


dupTemp <- dfRusTemp[
  duplicated(dfRusTemp$DateTime) |
  duplicated(dfRusTemp$DateTime, fromLast = TRUE),
]
dupTemp <- dupTemp[order(dupTemp$DateTime),]
unique(dupTemp$sensorid)

dupSal <- dfRusSal[
  duplicated(dfRusSal$DateTime) |
  duplicated(dfRusSal$DateTime, fromLast = TRUE),
]
dupSal <- dupSal[order(dupSal$DateTime),]
unique(dupSal$sensortype)
unique(dupSal$profile)

dupO2<-dfRusO2[
  duplicated(dfRusO2$DateTime) |
  duplicated(dfRusO2$DateTime, fromLast = TRUE),
]
dupO2 <- dupO2[order(dupO2$DateTime),]
unique(dupO2$sensorid)
sum(dupO2$sensorid=="791868")
sum(dupO2$sensorid=="317269")
sum(dfRusO2$sensorid=="317269")
sum(dfRusO2$sensorid=="791868")







### Set up time windows and plots
#Window One
timeStartWindowOne<-as.POSIXct("2025-10-20", tz = "UTC")
timeEndWindowOne<-as.POSIXct("2025-11-01", tz = "UTC")

# Depth
dfRusDepthWindowOne <- dfRusDepth[dfRusDepth$DateTime >= timeStartWindowOne & dfRusDepth$DateTime <  timeEndWindowOne,]
plot1<-ggplot(dfRusDepthWindowOne, aes(x = DateTime, y = rawWaterDepthMeters)) +
  geom_line() +
  labs(
    title = paste0("Russian River Water Depth Between ",format(timeStartWindowOne,"%B %Y")," and ",format(timeEndWindowOne, "%B %Y")),
    x = "Date",
    y = "Water Depth (m)"
  ) +
  theme_minimal()
plot1

# Temperature
dfRusTempWindowOne <- dfRusTemp[dfRusTemp$DateTime >= timeStartWindowOne & dfRusTemp$DateTime <  timeEndWindowOne,]
plot2<-ggplot(dfRusTempWindowOne, aes(x = DateTime, y = raw_h2otemp)) +
  geom_line() +
  labs(
    title = paste0("Russian River Water Temp Between ",format(timeStartWindowOne,"%B %Y")," and ",format(timeEndWindowOne, "%B %Y")),
    x = "Date",
    y = "Water Temp (deg C))"
  ) +
  theme_minimal()
plot2

# Salinity
dfRusSalWindowOne <- dfRusSal[dfRusSal$DateTime >= timeStartWindowOne & dfRusSal$DateTime <  timeEndWindowOne,]
plot3<-ggplot(dfRusSalWindowOne, aes(x = DateTime, y = raw_conductivity)) +
  geom_line() +
  labs(
    title = paste0("Russian River Conductivity Between ",format(timeStartWindowOne,"%B %Y")," and ",format(timeEndWindowOne, "%B %Y")),
    x = "Date",
    y = "Measured Conductivity (mS)"
  ) +
  theme_minimal()
plot3

#O2
dfRusO2WindowOne <- dfRusO2[dfRusO2$DateTime >= timeStartWindowOne & dfRusO2$DateTime <  timeEndWindowOne,]
plot4<-ggplot(dfRusO2WindowOne, aes(x = DateTime, y = raw_do)) +
  geom_line() +
  labs(
    title = paste0("Russian River Disolved O2 Between ",format(timeStartWindowOne,"%B %Y")," and ",format(timeEndWindowOne, "%B %Y")),
    x = "Date",
    y = "Measured Disolved O2 (mg/L") +
  theme_minimal()
plot4

#plot all 4 graphs 1 above the other 
plot1/plot2/plot3/plot4



 



# Old Code before 2026-06-09
# 
# #look at the odd depth readings from jan to march 2005
# timeStartWindow<-as.POSIXct("2025-01-19", tz = "UTC")
# timeEndWindow<-as.POSIXct("2025-03-11", tz = "UTC")
# dfRusOddJan <- dfRus2[dfRus2$DateTime >= timeStartWindow &dfRus2$DateTime <  timeEndWindow,]
# ggplot(dfRusOddJan, aes(x = DateTime, y = rawWaterDepthMeters)) +
#   geom_line() +
#   labs(
#     title = "Odd Depth readings in early 2025 at Russian River",
#     x = "Date",
#     y = "Water Depth (m)"
#   ) +
#   theme_minimal()
# unique(dfRusOddJan$raw_pressure)
# 
# timeStartWindow<-as.POSIXct("2025-06-06", tz = "UTC")
# timeEndWindow<-as.POSIXct("3000-01-01", tz = "UTC")
# dfRusOddJan <- dfRus2[dfRus2$DateTime >= timeStartWindow &dfRus2$DateTime <  timeEndWindow,]
# 
# ggplot(dfRusOddJune, aes(x = DateTime, y = rawWaterDepthMeters)) +
#   geom_line() +
#   labs(
#     title = "Odd Depth readings in late 2025 at Rus",
#     x = "Date",
#     y = "Water Depth (m)"
#   ) +
#   theme_minimal()
# unique(dfRusOddJan$raw_pressure)
# 
# 
# ### Because of odd readings from Jan to March in 2005 and very odd readings after July 2005, only look at 2004 data
# timeStartWindow<-as.POSIXct("2024-01-01", tz = "UTC")
# timeEndWindow<-as.POSIXct("2025-01-14", tz = "UTC")
# dfRus2_2024 <- dfRus2[
#   dfRus2$DateTime >= timeStartWindow &
#   dfRus2$DateTime <  timeEndWindow,
# ]
# 
# 
# ### find max depth and make a 60 day window
# maxWaterRow <- which.max(dfRus2_2024$rawWaterDepthMeters)
# maxWaterTime <- dfRus2_2024$DateTime[maxWaterRow]
# timePeakWindowStart <- (maxWaterTime - 30 * 24 * 60 * 60) 
# timePeakWindowEnd <- (maxWaterTime + 30 * 24 * 60 * 60)
# 
# dfPeakWindowDepth <- dfRus2_2024[
#   dfRus2_2024$DateTime >= timePeakWindowStart &
#   dfRus2_2024$DateTime <= timePeakWindowEnd,
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
#     title = "Russian River Water Depth 60 Day Window Near Peak 2024",
#     x = "Date",
#     y = "Water Depth (m)"
#   ) +
#   theme_minimal()
# 
# ### find max depth and make a 60 day window in spring/summer
# dfRus2_2024MarSept <- dfRus2[
#   format(dfRus2$DateTime, "%Y") == "2024" &
#     as.numeric(format(dfRus2$DateTime, "%m")) >= 3 &
#     as.numeric(format(dfRus2$DateTime, "%m")) <= 9,
# ]
# maxWaterSummerRow <- which.max(dfRus2_2024MarSept$rawWaterDepthMeters)
# maxWaterSummerTime <- dfRus2_2024MarSept$DateTime[maxWaterSummerRow]
# timeSummerPeakWindowStart <- (maxWaterSummerTime - 30 * 24 * 60 * 60) 
# timeSummerPeakWindowEnd <- (maxWaterSummerTime + 30 * 24 * 60 * 60)
# 
# dfSummerPeakWindowDepth <- dfRus2_2024MarSept[
#   dfRus2_2024MarSept$DateTime >= timeSummerPeakWindowStart &
#   dfRus2_2024MarSept$DateTime <= timeSummerPeakWindowEnd,
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
#     title = "Russian River Water Depth 60 Day Window Near Spring/Summer Peak Depth 2024",
#     x = "Date",
#     y = "Water Depth (m)"
#   ) +
#   theme_minimal()







#plot all 4 graphs 1 above the other
plot1/plot2/plot3/plot4


#Garbage Collector
gc()
