# install.packages("dplyr")
library(dplyr)

# Windows
strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/rawData/stateParks"
strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/cleanData/stateParks"
strMetaPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/metadata"

# # Linux
# strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/rawData/stateParks"
# strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/cleanData/stateParks"
# strMetaPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/metadata"

dir.create(strOutPath, recursive = TRUE, showWarnings = FALSE)

fileList <- list.files(strInPath, pattern = "\\.csv$", ignore.case = TRUE)

strMetaFilename <- "metadataStatePark.csv"

dfCoordinates <- read.csv(
  file.path(strMetaPath, strMetaFilename),
  stringsAsFactors = FALSE
)

############################################################
### Helper Functions
############################################################

### Pull sensor type and sensor id from a State Parks sensor column name.
parseSensorInfo <- function(strColumnName, strMeasurementPrefix) {
  strPrefixPattern <- paste0("^", gsub("\\\\.", "\\\\\\\\.", strMeasurementPrefix), "\\\\.?")
  strRemainder <- sub(strPrefixPattern, "", strColumnName)
  strRemainder <- gsub("^\\.+|\\.+$", "", strRemainder)
  strRemainder <- gsub("_+", ".", strRemainder)
  sensorParts <- unlist(strsplit(strRemainder, "\\."))
  sensorParts <- sensorParts[sensorParts != ""]
  
  sensorId <- NA_character_
  sensorType <- NA_character_
  
  if (length(sensorParts) > 0) {
    idCandidates <- grep("[0-9]", sensorParts)
    
    if (length(idCandidates) > 0) {
      idIndex <- tail(idCandidates, 1)
      sensorId <- sensorParts[idIndex]
      sensorTypeParts <- sensorParts[-idIndex]
    } else {
      sensorTypeParts <- sensorParts
    }
    
    if (length(sensorTypeParts) > 0) {
      sensorType <- paste(sensorTypeParts, collapse = " ")
    }
  }
  
  data.frame(
    sensorType = sensorType,
    sensorId = sensorId,
    stringsAsFactors = FALSE
  )
}

### Collapse matching sensor columns into one raw value and source metadata.
collapseSensorColumns <- function(df, strPattern, strMeasurementPrefix) {
  matchedCols <- grep(strPattern, names(df), value = TRUE)
  
  dfCollapsed <- data.frame(
    rawValue = rep(NA_real_, nrow(df)),
    sourceColumn = rep(NA_character_, nrow(df)),
    sensorType = rep(NA_character_, nrow(df)),
    sensorId = rep(NA_character_, nrow(df)),
    stringsAsFactors = FALSE
  )
  
  if (length(matchedCols) == 0) {
    attr(dfCollapsed, "matchedCols") <- matchedCols
    return(dfCollapsed)
  }
  
  for (colName in matchedCols) {
    sensorInfo <- parseSensorInfo(colName, strMeasurementPrefix)
    colValues <- suppressWarnings(as.numeric(df[[colName]]))
    idxUse <- is.na(dfCollapsed$rawValue) & !is.na(colValues)
    
    dfCollapsed$rawValue[idxUse] <- colValues[idxUse]
    dfCollapsed$sourceColumn[idxUse] <- colName
    dfCollapsed$sensorType[idxUse] <- sensorInfo$sensorType
    dfCollapsed$sensorId[idxUse] <- sensorInfo$sensorId
  }
  
  attr(dfCollapsed, "matchedCols") <- matchedCols
  dfCollapsed
}

### Add collapsed raw variable and sensor source columns.
addCollapsedSensorVariable <- function(df, strPattern, strMeasurementPrefix, strRawName) {
  dfCollapsed <- collapseSensorColumns(df, strPattern, strMeasurementPrefix)
  
  df[[strRawName]] <- dfCollapsed$rawValue
  df[[paste0(strRawName, "_source_column")]] <- dfCollapsed$sourceColumn
  df[[paste0(strRawName, "_sensor_type")]] <- dfCollapsed$sensorType
  df[[paste0(strRawName, "_sensor_id")]] <- dfCollapsed$sensorId
  
  attr(df, "matchedCols") <- attr(dfCollapsed, "matchedCols")
  df
}

############################################################
### Clean Files
############################################################

for (index in seq_along(fileList)) {
  
  fileName <- fileList[index]
  print(fileName)
  
  strFullName <- file.path(strInPath, fileName)
  dfImport <- read.csv(strFullName, stringsAsFactors = FALSE)
  
  dfImport$projectid <- "State-Parks"
  
  estuaryName <- fileName
  estuaryName <- sub("(2024|2025|2026).*", "", estuaryName)
  estuaryName <- gsub("([a-z])([A-Z])", "\\1 \\2", estuaryName)
  estuaryName <- paste0(
    toupper(substr(estuaryName, 1, 1)),
    substr(estuaryName, 2, nchar(estuaryName))
  )
  
  dfImport$estuaryname <- estuaryName
  
  ### Add metadata
  dfImport <- dfImport %>%
    left_join(dfCoordinates, by = "estuaryname")
  
  ### Create DateTime column
  dfImport$DateTime <- as.POSIXct(
    dfImport$Date,
    format = "%m/%d/%y %H:%M:%S %z",
    tz = "UTC"
  )
  
  ### Collapse sensor-specific columns into standard raw columns
  allSensorSourceCols <- character(0)
  
  dfImport <- addCollapsedSensorVariable(
    df = dfImport,
    strPattern = "Water\\.Temperature",
    strMeasurementPrefix = "Water.Temperature",
    strRawName = "raw_h2otemp"
  )
  allSensorSourceCols <- c(allSensorSourceCols, attr(dfImport, "matchedCols"))
  
  dfImport <- addCollapsedSensorVariable(
    df = dfImport,
    strPattern = "Water\\.Pressure",
    strMeasurementPrefix = "Water.Pressure",
    strRawName = "raw_pressure"
  )
  allSensorSourceCols <- c(allSensorSourceCols, attr(dfImport, "matchedCols"))
  
  dfImport <- addCollapsedSensorVariable(
    df = dfImport,
    strPattern = "Diff\\.Pressure",
    strMeasurementPrefix = "Diff.Pressure",
    strRawName = "raw_depth"
  )
  allSensorSourceCols <- c(allSensorSourceCols, attr(dfImport, "matchedCols"))
  
  dfImport <- addCollapsedSensorVariable(
    df = dfImport,
    strPattern = "Water\\.Level",
    strMeasurementPrefix = "Water.Level",
    strRawName = "raw_water_level"
  )
  allSensorSourceCols <- c(allSensorSourceCols, attr(dfImport, "matchedCols"))
  
  dfImport <- addCollapsedSensorVariable(
    df = dfImport,
    strPattern = "Barometric\\.Pressure",
    strMeasurementPrefix = "Barometric.Pressure",
    strRawName = "raw_atmospheric_pressure"
  )
  allSensorSourceCols <- c(allSensorSourceCols, attr(dfImport, "matchedCols"))
  
  allSensorSourceCols <- unique(allSensorSourceCols)
  
  if (length(allSensorSourceCols) > 0) {
    dfImport <- dfImport %>%
      select(-all_of(allSensorSourceCols))
  }
  
  attr(dfImport, "matchedCols") <- NULL
  
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
