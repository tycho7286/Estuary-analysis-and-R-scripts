### install and load packages
# install.packages("dplyr")
# install.packages("oce")
library(dplyr)
library(gsw)
library(oce)
library(ggplot2)
library(ggrastr)

#load in variables and read datafile
# # Windows
# strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataCombined"
# strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"

# Linux
strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataCombined"
strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"

strReadFilename <- "combinedDataset.csv"
strWriteFilename <- "datasetWorkingCopy.csv"
strFullName <- paste0(strInPath,"/",strReadFilename)
index = 0
depthVsPressureIntercept <- 3.837748327e-04 #Copied from previous Model Depth in meters pressure in cmH2O
depthVsPressureSlope <- 9.881125021e-03 #Copied from previous Model Depth in meters pressure in cmH2O
intConductivityConstant <- 42.914 #for the oce package salinity conversion
intCmH2OToDbar <- 101.971621297793 #for the oce package salinity conversion
#estuaryCombined <- read.csv(strFullName)





##inspect units
# print(unique(estuaryCombined$sensortype))
# sum(estuaryCombined$sensortype == strSensorType)
# dfSubset <- estuaryCombined[estuaryCombined$sensortype== strSensorType, ]
# print(unique(dfSubset$raw_pressure_unit))
# print(unique(dfSubset$raw_h2otemp_unit))
# print(unique(dfSubset$raw_do_unit))
# print(unique(dfSubset$raw_conductivity_unit))
# print(unique(dfSubset$raw_salinity_unit))
# 
# # various inspection commands
# print(unique(dfSubset$projectid))
# print(unique(estuaryCombined$wqnotes))
# print(unique(estuaryCombined$raw_depth_unit))
# sum(estuaryCombined$raw_depth_unit=="m")
# sum(estuaryCombined$rawWaterDepthMeters_units=="m")
# sum(estuaryCombined$rawWaterDepthMeters!="")
# unique(estuaryCombined$organization)
# unique(estuaryCombined$sensorlocation)
# unique(estuaryCombined$wqnotes)
# unique(estuaryCombined$qaqc_comment)
# dfSubset <- estuaryCombined[
#   !is.na(estuaryCombined$sensorlocation) &
#     estuaryCombined$sensorlocation == "Channel",
# ]
# 
# sum(estuaryCombined$sensorlocation=="Channel",na.rm=TRUE)


###Adjust all raw depths to units of meters
estuaryCombined["rawWaterDepthMeters"] <- NA_real_
estuaryCombined["rawWaterDepthMeters_units"] <- "Not Recorded"
print(unique(estuaryCombined$rawWaterDepthMeters))
sum(estuaryCombined$rawWaterDepthMeters=="Not Recorded")
sum(estuaryCombined$raw_depth_unit=="m")+sum(estuaryCombined$raw_depth_unit=="cm")
estuaryCombined$rawWaterDepthMeters[estuaryCombined$raw_depth_unit == "m"] <- estuaryCombined$raw_depth[estuaryCombined$raw_depth_unit == "m"]
estuaryCombined$rawWaterDepthMeters_units[estuaryCombined$raw_depth_unit == "m"] <- estuaryCombined$raw_depth_unit[estuaryCombined$raw_depth_unit == "m"]
estuaryCombined$rawWaterDepthMeters[estuaryCombined$raw_depth_unit == "cm"] <- estuaryCombined$raw_depth[estuaryCombined$raw_depth_unit == "cm"]/100
estuaryCombined$rawWaterDepthMeters_units[estuaryCombined$raw_depth_unit == "cm"] <- "m"
print(unique(estuaryCombined$rawWaterDepthMeters))
print(unique(estuaryCombined$rawWaterDepthMeters_units))
sum(!is.na(estuaryCombined$rawWaterDepthMeters))
sum(estuaryCombined$rawWaterDepthMeters_units!="Not Recorded")
sum(is.na(estuaryCombined$rawWaterDepthMeters))
sum(estuaryCombined$rawWaterDepthMeters_units=="Not Recorded")


###Adjust all raw pressure to units of cm H2O
estuaryCombined["rawPressureCm"] <- NA_real_
estuaryCombined["rawPressureCm_unit"] <- "Not Recorded"
print(unique(estuaryCombined$rawPressureCm))
sum(estuaryCombined$rawPressureCm=="Not Recorded")
unique(estuaryCombined$raw_pressure_unit)
sum(estuaryCombined$raw_pressure_unit=="cmH2O")+sum(estuaryCombined$raw_pressure_unit=="mbar")+sum(estuaryCombined$raw_pressure_unit=="psi")+sum(estuaryCombined$raw_pressure_unit=="kPa")+sum(estuaryCombined$raw_pressure_unit=="dbar")

#copy cmH2O
estuaryCombined$rawPressureCm[estuaryCombined$raw_pressure_unit == "cmH2O"] <- estuaryCombined$raw_pressure[estuaryCombined$raw_pressure_unit == "cmH2O"]
estuaryCombined$rawPressureCm_unit[estuaryCombined$raw_pressure_unit == "cmH2O"] <- "cmH2O"

#convert mbar
estuaryCombined$rawPressureCm[estuaryCombined$raw_pressure_unit == "mbar"] <- estuaryCombined$raw_pressure[estuaryCombined$raw_pressure_unit == "mbar"]*1.01971621297793
estuaryCombined$rawPressureCm_unit[estuaryCombined$raw_pressure_unit == "mbar"] <- "cmH2O"

#convert psi
estuaryCombined$rawPressureCm[estuaryCombined$raw_pressure_unit == "psi"] <- estuaryCombined$raw_pressure[estuaryCombined$raw_pressure_unit == "psi"]*70.3069579640171
estuaryCombined$rawPressureCm_unit[estuaryCombined$raw_pressure_unit == "psi"] <- "cmH2O"

#convert kPa
estuaryCombined$rawPressureCm[estuaryCombined$raw_pressure_unit == "kPa"] <- estuaryCombined$raw_pressure[estuaryCombined$raw_pressure_unit == "kPa"]*10.1971621297793
estuaryCombined$rawPressureCm_unit[estuaryCombined$raw_pressure_unit == "kPa"] <- "cmH2O"

#convert dbar
estuaryCombined$rawPressureCm[estuaryCombined$raw_pressure_unit == "dbar"] <- estuaryCombined$raw_pressure[estuaryCombined$raw_pressure_unit == "dbar"]*101.971621297793
estuaryCombined$rawPressureCm_unit[estuaryCombined$raw_pressure_unit == "dbar"] <- "cmH2O"

print(unique(estuaryCombined$rawPressureCm))
print(unique(estuaryCombined$rawPressureCm_unit))
sum(!is.na(estuaryCombined$rawPressureCm))
sum(estuaryCombined$rawPressureCm_unit!="Not Recorded")
sum(estuaryCombined$raw_pressure_unit=="cmH2O")+sum(estuaryCombined$raw_pressure_unit=="mbar")+sum(estuaryCombined$raw_pressure_unit=="psi")+sum(estuaryCombined$raw_pressure_unit=="kPa")+sum(estuaryCombined$raw_pressure_unit=="dbar")


###Calculate depth from pressure
estuaryCombined["calculatedWaterDepthMeters"] <- NA_real_
estuaryCombined$calculatedWaterDepthMeters <- estuaryCombined$rawPressureCm*depthVsPressureSlope+depthVsPressureIntercept
sum(is.na(estuaryCombined$calculatedWaterDepthMeters))
sum(is.na(estuaryCombined$rawPressureCm))
sum(!is.na(estuaryCombined$calculatedWaterDepthMeters))
sum(!is.na(estuaryCombined$rawPressureCm))


###Calculate salinity from pressure and temperature
estuaryCombined$calculatedSalPSU <- NA_real_

#Remove -88 conductivity Values
bad_idx <- estuaryCombined$raw_conductivity < 0
# estuaryCombined$raw_conductivity[bad_idx] <- NA
estuaryCombined$raw_conductivity_unit[bad_idx] <- "Apparnet Sensor Error"
# unique(estuaryCombined$raw_salinity[bad_idx])

idx_mS <- estuaryCombined$raw_conductivity_unit == "mS/cm" &
  !is.na(estuaryCombined$raw_conductivity) &
  !is.na(estuaryCombined$raw_h2otemp) &
  !is.na(estuaryCombined$rawPressureCm)

idx_uS <- estuaryCombined$raw_conductivity_unit == "uS/cm" &
  !is.na(estuaryCombined$raw_conductivity) &
  !is.na(estuaryCombined$raw_h2otemp) &
  !is.na(estuaryCombined$rawPressureCm)

estuaryCombined$calculatedSalPSU[idx_mS] <- swSCTp(
  conductivity = estuaryCombined$raw_conductivity[idx_mS] / intConductivityConstant,
  temperature = estuaryCombined$raw_h2otemp[idx_mS],
  pressure = estuaryCombined$rawPressureCm[idx_mS] / intCmH2OToDbar
)

estuaryCombined$calculatedSalPSU[idx_uS] <- swSCTp(
  conductivity = (estuaryCombined$raw_conductivity[idx_uS] / 1000) / intConductivityConstant,
  temperature = estuaryCombined$raw_h2otemp[idx_uS],
  pressure = estuaryCombined$rawPressureCm[idx_uS] / intCmH2OToDbar
)



###Filter DO Pct values for only values above 0%

bad_idx <- !is.na(estuaryCombined$raw_do_pct) &
  estuaryCombined$raw_do_pct <= 0

estuaryCombined$raw_do_pct[bad_idx] <- "Apparent Sensor Error (Negative DO Pct value)"

estuaryCombined$calculatedDOPct <- NA_real_

good_idx <- !is.na(estuaryCombined$raw_do_pct) &
  estuaryCombined$raw_do_pct > 0

estuaryCombined$calculatedDOPct[good_idx] <-
  estuaryCombined$raw_do_pct[good_idx]

### Make all dates POSIX
estuaryCombined$DateTime <- NA_real_
estuaryCombined$DateTime <- as.POSIXct(
  ifelse(
    !is.na(estuaryCombined$time_utc) &
      estuaryCombined$time_utc != "",
    estuaryCombined$time_utc,
    estuaryCombined$time
  ),
  format = "%Y-%m-%dT%H:%M:%SZ",
  tz = "UTC"
)






###Write out dataFile
strFullWriteName <- paste0(strOutPath,"/",strWriteFilename)
print(strFullWriteName)
write.csv(estuaryCombined, strFullWriteName, row.names = FALSE)


# lmSalComp<-lm(raw_salinity~calculatedSalPSU, data=dfSalComparison)
# ggplot(dfSalComparison, aes(x = calculatedSalPSU, y = raw_salinity)) +
#   geom_bin2d(bins = 200) +
#   geom_abline(intercept = coef(lmSalComp)[1], slope = coef(lmSalComp)[2], color = "red", linewidth = 1) +
#   labs(title = "Raw Salinity vs Calculated Salinity") +
#   theme_minimal()
# summary(lmSalComp)

# 
# range(estuaryCombined$calculatedSalPSU ,na.rm=TRUE)
# unique(estuaryCombined$raw_conductivity_unit)
# table(
#   estuaryCombined$raw_conductivity_unit,
# #  !is.na(estuaryCombined$raw_conductivity)
# #  estuaryCombined$raw_conductivity < 0
#   estuaryCombined$calculatedSalPSU < 80
# )
# 
# sum(estuaryCombined$raw_conductivity_unit=="Not Recorded")
# sum(estuaryCombined$raw_conductivity_unit=="mS/cm")
# sum(estuaryCombined$raw_conductivity_unit=="uS/cm")
# fivenum(estuaryCombined$raw_conductivity)
# sum(!is.na(estuaryCombined$raw_conductivity))
# sum(is.na(estuaryCombined$raw_conductivity))
# sum(estuaryCombined$raw_conductivity == -88,na.rm=TRUE)
# sum(estuaryCombined$raw_conductivity < 0,na.rm=TRUE)
# sum(
#   estuaryCombined$raw_salinity < 0 &
#     !is.na(estuaryCombined$calculatedSalPSU),
#   na.rm = TRUE
# )
# 
# 
# ### Model and Plot raw salinity vs calculated salinity
# dfSalComparison <- estuaryCombined[!is.na(estuaryCombined$raw_salinity)&estuaryCombined$raw_salinity > 0,]
# 
# lmSalComp<-lm(raw_salinity~calculatedSalPSU, data=dfSalComparison)
# ggplot(dfSalComparison, aes(x = calculatedSalPSU, y = raw_salinity)) +
#   geom_bin2d(bins = 200) +
#   geom_abline(intercept = coef(lmSalComp)[1], slope = coef(lmSalComp)[2], color = "red", linewidth = 1) +
#   labs(title = "Raw Salinity vs Calculated Salinity") +
#   theme_minimal()
# summary(lmSalComp)
# 
# ggplot(estuaryCombined, aes(x = raw_h2otemp, y = raw_salinity)) +
#   geom_bin2d(bins = 200) +
#   #  geom_abline(intercept = coef(lmSalComp)[1], slope = coef(lmSalComp)[2], color = "red", linewidth = 1) +
#   labs(title = "Raw Salintiy vs Temperature") +
#   theme_minimal()
# 
# 
# ggplot(estuaryCombined, aes(x = raw_h2otemp, y = calculatedSalPSU)) +
#   geom_bin2d(bins = 200) +
#   #  geom_abline(intercept = coef(lmSalComp)[1], slope = coef(lmSalComp)[2], color = "red", linewidth = 1) +
#   labs(title = "Calculated Salinity vs Temperature") +
#   theme_minimal()
# 
# 
# fivenum(estuaryCombined$raw_salinity[estuaryCombined$raw_salinity_unit=="Not Recorded"])
# max(estuaryCombined$raw_salinity[estuaryCombined$raw_salinity_unit=="Not Recorded"])
# 





### what each sensor records:
# barometer         =   pressure in cmH2O and temp in deg C
# CTD               =   pressure in cmH2O or dbar, temp in deg C, and conductivity in mS/cm, Not Recorded, or uS/cm 
# tidbit            =   temp in deg C (assume at surface)
# minidot           =   temp in deg C or Not Recorded, do in mg/L or Not Recorded
# troll             =   pressure in mbar or psi, temp in deg C
# ADCP              =   temp in deg C or Not Recorded
# EXO               =   pressure in mbar, psi, or Not Recorded, temp in deg C, do in Not Recorded or mg/L, conductivity in uS/cm
# HOBO RX2100       =   pressure in kPa, temp in deg C
# HL Series Sensors =   temp in deg C, do in mg/L, conductivity in mS/cm, uS/cm and Not Recorded
# unknown           =   has a wierd comment and bad pressure data, removing from dataset








