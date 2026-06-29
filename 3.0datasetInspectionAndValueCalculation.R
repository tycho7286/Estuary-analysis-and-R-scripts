### install and load packages
#install.packages("dplyr")
#install.packages("gsw")
#install.packages("oce")
#install.packages("ggplot2")
#install.packages("ggrastr")

library(dplyr)
library(gsw)
library(oce)
library(ggplot2)
library(ggrastr)

### load in variables and read datafile
# Windows
strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataCombined"
strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"

# # Linux
# strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataCombined"
# strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"

strReadFilename <- "combinedDataset.rds"
strWriteFilename <- "datasetWorkingCopy.rds"

strFullName <- file.path(strInPath, strReadFilename)

depthVsPressureIntercept <- 3.837748327e-04
depthVsPressureSlope <- 9.881125021e-03
intConductivityConstant <- 42.914
intCmH2OToDbar <- 101.971621297793

dir.create(strOutPath, recursive = TRUE, showWarnings = FALSE)

### Read combined dataset
estuaryCombined <- readRDS(strFullName)

### Adjust all raw depths to units of meters
estuaryCombined$rawWaterDepthMeters <- NA_real_
estuaryCombined$rawWaterDepthMeters_units <- "Not Recorded"

idx_m <- !is.na(estuaryCombined$raw_depth_unit) & estuaryCombined$raw_depth_unit == "m"

idx_cm <- !is.na(estuaryCombined$raw_depth_unit) & estuaryCombined$raw_depth_unit == "cm"

estuaryCombined$rawWaterDepthMeters[idx_m] <- estuaryCombined$raw_depth[idx_m]

estuaryCombined$rawWaterDepthMeters_units[idx_m] <- "m"

estuaryCombined$rawWaterDepthMeters[idx_cm] <- estuaryCombined$raw_depth[idx_cm] / 100

estuaryCombined$rawWaterDepthMeters_units[idx_cm] <- "m"

print("Raw Depth")

### Adjust all raw pressure to units of cm H2O
estuaryCombined$rawPressureCm <- NA_real_
estuaryCombined$rawPressureCm_unit <- "Not Recorded"

idx_cmH2O <- !is.na(estuaryCombined$raw_pressure_unit) & estuaryCombined$raw_pressure_unit == "cmH2O"

idx_mbar <- !is.na(estuaryCombined$raw_pressure_unit) & estuaryCombined$raw_pressure_unit == "mbar"

idx_psi <- !is.na(estuaryCombined$raw_pressure_unit) & estuaryCombined$raw_pressure_unit == "psi"

idx_kPa <- !is.na(estuaryCombined$raw_pressure_unit) & estuaryCombined$raw_pressure_unit == "kPa"

idx_dbar <- !is.na(estuaryCombined$raw_pressure_unit) & estuaryCombined$raw_pressure_unit == "dbar"

estuaryCombined$rawPressureCm[idx_cmH2O] <- estuaryCombined$raw_pressure[idx_cmH2O]

estuaryCombined$rawPressureCm_unit[idx_cmH2O] <- "cmH2O"

estuaryCombined$rawPressureCm[idx_mbar] <- estuaryCombined$raw_pressure[idx_mbar] * 1.01971621297793

estuaryCombined$rawPressureCm_unit[idx_mbar] <- "cmH2O"

estuaryCombined$rawPressureCm[idx_psi] <- estuaryCombined$raw_pressure[idx_psi] * 70.3069579640171

estuaryCombined$rawPressureCm_unit[idx_psi] <- "cmH2O"

estuaryCombined$rawPressureCm[idx_kPa] <- estuaryCombined$raw_pressure[idx_kPa] * 10.1971621297793

estuaryCombined$rawPressureCm_unit[idx_kPa] <- "cmH2O"

estuaryCombined$rawPressureCm[idx_dbar] <- estuaryCombined$raw_pressure[idx_dbar] * 101.971621297793

estuaryCombined$rawPressureCm_unit[idx_dbar] <- "cmH2O"

print("Raw Pressure")

### Calculate depth from pressure
estuaryCombined$calculatedWaterDepthMeters <- NA_real_

estuaryCombined$calculatedWaterDepthMeters <- estuaryCombined$rawPressureCm *
  depthVsPressureSlope +
  depthVsPressureIntercept

print("Calc Depth")

### Calculate salinity from conductivity, temperature, and pressure
estuaryCombined$calculatedSalPSU <- NA_real_

bad_idx <- !is.na(estuaryCombined$raw_conductivity) & estuaryCombined$raw_conductivity < 0

estuaryCombined$raw_conductivity_unit[bad_idx] <- "Apparent Sensor Error"

idx_mS <- !is.na(estuaryCombined$raw_conductivity_unit) &
  estuaryCombined$raw_conductivity_unit == "mS/cm" &
  !is.na(estuaryCombined$raw_conductivity) & !is.na(estuaryCombined$raw_h2otemp) &
  !is.na(estuaryCombined$rawPressureCm)

idx_uS <- !is.na(estuaryCombined$raw_conductivity_unit) &
  estuaryCombined$raw_conductivity_unit == "uS/cm" &
  !is.na(estuaryCombined$raw_conductivity) &
  !is.na(estuaryCombined$raw_h2otemp) &
  !is.na(estuaryCombined$rawPressureCm)

estuaryCombined$calculatedSalPSU[idx_mS] <- swSCTp(
  conductivity = estuaryCombined$raw_conductivity[idx_mS] / intConductivityConstant,
  temperature = estuaryCombined$raw_h2otemp[idx_mS],
  pressure = estuaryCombined$rawPressureCm[idx_mS] / intCmH2OToDbar
)

estuaryCombined$calculatedSalPSU[idx_uS] <- swSCTp(
  conductivity =
    (estuaryCombined$raw_conductivity[idx_uS] / 1000) /
    intConductivityConstant,
  temperature =
    estuaryCombined$raw_h2otemp[idx_uS],
  pressure =
    estuaryCombined$rawPressureCm[idx_uS] /
    intCmH2OToDbar
)

print("Salinity")

### Filter DO Percent values
estuaryCombined$calculatedDOPct <- NA_real_

bad_idx <- !is.na(estuaryCombined$raw_do_pct) & estuaryCombined$raw_do_pct <= 0

estuaryCombined$raw_do_pct[bad_idx] <- NA

good_idx <- !is.na(estuaryCombined$raw_do_pct) & estuaryCombined$raw_do_pct > 0

estuaryCombined$calculatedDOPct[good_idx] <- estuaryCombined$raw_do_pct[good_idx]

print("DO Pct")

### Write out data file
strFullWriteName <- file.path(strOutPath, strWriteFilename)

saveRDS(estuaryCombined, strFullWriteName)

gc()