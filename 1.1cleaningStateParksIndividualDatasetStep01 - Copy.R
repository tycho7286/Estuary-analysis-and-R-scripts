# install.packages("dplyr")
library(dplyr)

# Windows
strInPath <- "C:/Users/Kevin/My Drive/School/2026Summer-BML-UCDGAP/Data/rawData/stateParks"
strOutPath <- "C:/Users/Kevin/My Drive/School/2026Summer-BML-UCDGAP/Data/cleanData/stateParks"

# # Linux
# strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/rawData/stateParks"
# strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/cleanData/stateParks"

dir.create(strOutPath, recursive = TRUE, showWarnings = FALSE)

fileList <- list.files(strInPath)

dfCoordinates <- data.frame(
  estuaryname = c(
    "Big Lagoon", "Lake Cleone", "Noyo River", "Big River", "Salmon Creek",
    "Bodega Harbor", "Tomalas Bay", "San Gregorio Creek", "Pescadero Lagoon", "Younger Lagoon",
    "Aptos Creek", "Devereux Slough", "Carpinteria Salt Marsh", "Santa Clara River", "Newport Bay",
    "Mission Bay"
  ),
  latitude = c(
    41.164028, 39.489500, 39.423639, 39.302750, 38.351522,
    38.332739, 38.107972, 37.320389, 37.261028, 36.950419,
    36.971111, 34.411864, 34.398583, 34.235100, 33.598711,
    32.794575
  ),
  longitude = c(
    -124.130667, -123.796389, -123.803556, -123.785611, -123.061917,
    -123.058725, -122.862306, -122.400861, -122.407750, -122.067050,
    -121.905611, -119.877183, -119.535278, -119.263219, -117.882714,
    -117.227644
  )
)

for (index in seq_along(fileList)) {
  
  fileName <- fileList[index]
  print(fileName)
  
  strFullName <- file.path(strInPath, fileName)
  dfImport <- read.csv(strFullName)
  
  dfImport$projectid <- "State-Parks"
  
  estuaryName <- fileName
  estuaryName <- sub("(2024|2025|2026).*", "", estuaryName)
  estuaryName <- gsub("([a-z])([A-Z])", "\\1 \\2", estuaryName)
  estuaryName <- paste0(
    toupper(substr(estuaryName, 1, 1)),
    substr(estuaryName, 2, nchar(estuaryName))
  )
  
  dfImport$estuaryname <- estuaryName
  
  ### Add coordinates
  dfImport <- dfImport %>%
    left_join(dfCoordinates, by = "estuaryname")
  
  ### Create DateTime column
  dfImport$DateTime <- as.POSIXct(
    dfImport$Date,
    format = "%m/%d/%y %H:%M:%S %z",
    tz = "UTC"
  )
  
  tempCols <- grep("Water\\.Temperature", names(dfImport), value = TRUE)
  if (length(tempCols) > 0) {
    dfImport$raw_h2otemp <- dfImport[[tempCols[1]]]
  } else {
    dfImport$raw_h2otemp <- NA
  }
  
  waterPressCols <- grep("Water\\.Pressure", names(dfImport), value = TRUE)
  if (length(waterPressCols) > 0) {
    dfImport$raw_pressure <- dfImport[[waterPressCols[1]]]
  } else {
    dfImport$raw_pressure <- NA
  }
  
  diffCols <- grep("Diff\\.Pressure", names(dfImport), value = TRUE)
  if (length(diffCols) > 0) {
    dfImport$raw_depth <- dfImport[[diffCols[1]]]
  } else {
    dfImport$raw_depth <- NA
  }
  
  waterLevelCols <- grep("Water\\.Level", names(dfImport), value = TRUE)
  if (length(waterLevelCols) > 0) {
    dfImport$raw_water_level <- dfImport[[waterLevelCols[1]]]
  } else {
    dfImport$raw_water_level <- NA
  }
  
  baroCols <- grep("Barometric\\.Pressure", names(dfImport), value = TRUE)
  if (length(baroCols) > 0) {
    dfImport$raw_atmospheric_pressure <- dfImport[[baroCols[1]]]
  } else {
    dfImport$raw_atmospheric_pressure <- NA
  }
  
  numericCols <- c(
    "raw_h2otemp",
    "raw_pressure",
    "raw_depth",
    "raw_water_level",
    "raw_atmospheric_pressure"
  )
  
  for (col in numericCols) {
    dfImport[[col]] <- suppressWarnings(as.numeric(dfImport[[col]]))
  }
  
  dfImport$raw_h2otemp_unit <- "Not Recorded"
  dfImport$raw_h2otemp_unit[!is.na(dfImport$raw_h2otemp)] <- "C"
  
  dfImport$raw_pressure_unit <- "Not Recorded"
  dfImport$raw_pressure_unit[!is.na(dfImport$raw_pressure)] <- "kPa"
  
  dfImport$raw_depth_unit <- "Not Recorded"
  dfImport$raw_depth_unit[!is.na(dfImport$raw_depth)] <- "kPa"
  
  dfImport$raw_water_level_unit <- "Not Recorded"
  dfImport$raw_water_level_unit[!is.na(dfImport$raw_water_level)] <- "m"
  
  dfImport$raw_atmospheric_pressure_unit <- "Not Recorded"
  dfImport$raw_atmospheric_pressure_unit[
    !is.na(dfImport$raw_atmospheric_pressure)
  ] <- "kPa"
  
  fullWriteName <- file.path(
    strOutPath,
    paste0(estuaryName, index, "CleaningStep01.rds")
  )
  
  saveRDS(dfImport, fullWriteName)
  
  cat(index, " ", estuaryName, "\n")
}

gc()