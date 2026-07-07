############################################################
### Install and Load Packages
############################################################

# install.packages("dplyr")
library(dplyr)

############################################################
### File Paths
############################################################

# Windows
strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/rawData/EMPA"
strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/cleanData/EMPA"
strMetadataPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/metadata"

# # Linux
# strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/rawData/EMPA"
# strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/cleanData/EMPA"
# strMetadataPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/metadata"

strGPSLookupFilename <- "empaSensorGPSLookup.csv"
strGPSMatchSummaryFilename <- "empaCleaningGPSMatchSummary.csv"

strFullGPSLookupName <- file.path(strMetadataPath, strGPSLookupFilename)
strFullGPSMatchSummaryName <- file.path(strMetadataPath, strGPSMatchSummaryFilename)

############################################################
### Helper Functions
############################################################

### Clean station numbers so 1 and 1.0 match correctly.
cleanStationNo <- function(stationNo) {
  stationNo <- as.character(stationNo)
  stationNo <- trimws(stationNo)
  stationNo <- sub("\\.0$", "", stationNo)
  stationNo
}

### Convert blank strings to NA.
blankToNA <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA
  x
}

### Parse EMPA ISO time strings as UTC POSIXct.
parseEMPATime <- function(timeValue) {
  as.POSIXct(
    timeValue,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
}

### Add empty GPS output fields to a data frame.
addEmptyGPSFields <- function(dfImport) {
  dfImport$gpsMatchStatus <- NA_character_
  dfImport$gpsMatchCount <- NA_integer_
  dfImport$gpsLatitude <- NA_real_
  dfImport$gpsLongitude <- NA_real_
  dfImport$gpsSource <- NA_character_
  dfImport$gpsStartDate <- as.POSIXct(NA, tz = "UTC")
  dfImport$gpsEndDate <- as.POSIXct(NA, tz = "UTC")
  dfImport$uniqueCoordinateEntry <- NA_integer_
  dfImport$finalCoordinateGroup <- NA_character_
  dfImport$finalCoordinateMethod <- NA_character_
  dfImport$combineToSinglePin <- NA_character_
  dfImport$dbscanCluster <- NA_integer_
  dfImport$dbscanClusterRadiusMeters <- NA_real_
  dfImport$dbscanClusterDiameterMeters <- NA_real_
  dfImport$dbscanClusterPointCount <- NA_real_
  dfImport$gpsReviewNotes <- NA_character_
  dfImport
}

### Assign GPS fields using sensor, profile, station, estuary, and DateTime range.
assignGPSByDate <- function(dfImport, dfGPSLookup) {
  dfImport <- addEmptyGPSFields(dfImport)
  
  for (rowIndex in seq_len(nrow(dfImport))) {
    currentDateTime <- dfImport$DateTime[rowIndex]
    
    if (is.na(currentDateTime)) {
      dfImport$gpsMatchStatus[rowIndex] <- "missing_observation_datetime"
      dfImport$gpsMatchCount[rowIndex] <- 0L
      next
    }
    
    dfCandidate <- dfGPSLookup[
      dfGPSLookup$estuaryname == dfImport$estuaryname[rowIndex] &
        dfGPSLookup$stationno == dfImport$stationno[rowIndex] &
        dfGPSLookup$sensorid == dfImport$sensorid[rowIndex] &
        dfGPSLookup$profile == dfImport$profile[rowIndex],
    ]
    
    if (nrow(dfCandidate) == 0) {
      dfImport$gpsMatchStatus[rowIndex] <- "no_sensor_profile_match"
      dfImport$gpsMatchCount[rowIndex] <- 0L
      next
    }
    
    idxDateMatch <- !is.na(dfCandidate$gpsStartDate) &
      dfCandidate$gpsStartDate <= currentDateTime &
      (
        is.na(dfCandidate$gpsEndDate) |
          dfCandidate$gpsEndDate >= currentDateTime
      )
    
    dfDateCandidate <- dfCandidate[idxDateMatch, ]
    matchCount <- nrow(dfDateCandidate)
    dfImport$gpsMatchCount[rowIndex] <- matchCount
    
    if (matchCount == 0) {
      dfImport$gpsMatchStatus[rowIndex] <- "no_date_range_match"
      next
    }
    
    if (matchCount > 1) {
      dfImport$gpsMatchStatus[rowIndex] <- "duplicate_date_range_match"
      next
    }
    
    dfMatch <- dfDateCandidate[1, ]
    
    dfImport$gpsMatchStatus[rowIndex] <- "matched"
    dfImport$gpsLatitude[rowIndex] <- dfMatch$finalLatitude
    dfImport$gpsLongitude[rowIndex] <- dfMatch$finalLongitude
    dfImport$gpsSource[rowIndex] <- dfMatch$gpsSource
    dfImport$gpsStartDate[rowIndex] <- dfMatch$gpsStartDate
    dfImport$gpsEndDate[rowIndex] <- dfMatch$gpsEndDate
    dfImport$uniqueCoordinateEntry[rowIndex] <- dfMatch$uniqueCoordinateEntry
    dfImport$finalCoordinateGroup[rowIndex] <- dfMatch$finalCoordinateGroup
    dfImport$finalCoordinateMethod[rowIndex] <- dfMatch$finalCoordinateMethod
    dfImport$combineToSinglePin[rowIndex] <- dfMatch$combineToSinglePin
    dfImport$dbscanCluster[rowIndex] <- dfMatch$dbscanCluster
    dfImport$dbscanClusterRadiusMeters[rowIndex] <- dfMatch$dbscanClusterRadiusMeters
    dfImport$dbscanClusterDiameterMeters[rowIndex] <- dfMatch$dbscanClusterDiameterMeters
    dfImport$dbscanClusterPointCount[rowIndex] <- dfMatch$dbscanClusterPointCount
    dfImport$gpsReviewNotes[rowIndex] <- dfMatch$gpsReviewNotes
  }
  
  dfImport
}

############################################################
### Load GPS Lookup
############################################################

dfGPSLookup <- read.csv(strFullGPSLookupName, stringsAsFactors = FALSE)

dfGPSLookup <- dfGPSLookup %>%
  mutate(
    estuaryname = trimws(as.character(estuaryname)),
    stationno = cleanStationNo(stationno),
    sensorid = trimws(as.character(sensorid)),
    profile = trimws(as.character(profile)),
    gpsStartDate = parseEMPATime(gpsStartDate),
    gpsEndDate = parseEMPATime(gpsEndDate),
    uniqueCoordinateEntry = suppressWarnings(as.integer(uniqueCoordinateEntry)),
    finalLatitude = suppressWarnings(as.numeric(finalLatitude)),
    finalLongitude = suppressWarnings(as.numeric(finalLongitude)),
    gpsSource = blankToNA(gpsSource),
    finalCoordinateGroup = blankToNA(finalCoordinateGroup),
    finalCoordinateMethod = blankToNA(finalCoordinateMethod),
    combineToSinglePin = blankToNA(combineToSinglePin),
    dbscanCluster = suppressWarnings(as.integer(dbscanCluster)),
    dbscanClusterRadiusMeters = suppressWarnings(as.numeric(dbscanClusterRadiusMeters)),
    dbscanClusterDiameterMeters = suppressWarnings(as.numeric(dbscanClusterDiameterMeters)),
    dbscanClusterPointCount = suppressWarnings(as.numeric(dbscanClusterPointCount)),
    gpsReviewNotes = blankToNA(gpsReviewNotes)
  ) %>%
  filter(
    !is.na(estuaryname),
    estuaryname != "",
    !is.na(stationno),
    stationno != "",
    !is.na(sensorid),
    sensorid != "",
    !is.na(profile),
    profile != "",
    !is.na(finalLatitude),
    !is.na(finalLongitude)
  )

############################################################
### Get File Names
############################################################

dir.create(strOutPath, recursive = TRUE, showWarnings = FALSE)

fileList <- list.files(strInPath, pattern = "\\.csv$", ignore.case = TRUE)
gpsMatchSummaryList <- list()
index <- 0

############################################################
### Import, Clean, Add GPS, and Save Each EMPA File
############################################################

for (i in fileList) {
  index <- index + 1
  strFullName <- file.path(strInPath, i)
  
  dfImport <- read.csv(strFullName, stringsAsFactors = FALSE) %>%
    mutate(
      sensorid = trimws(as.character(sensorid)),
      stationno = cleanStationNo(stationno),
      profile = trimws(as.character(profile)),
      estuaryname = trimws(as.character(estuaryname)),
      sensortype = trimws(as.character(sensortype))
    )
  
  estuaryName <- paste0("estuary-", substr(i, start = 1, stop = 7))
  fullWriteName <- file.path(strOutPath, paste0(estuaryName, "CleaningStep01.rds"))
  
  ############################################################
  ### Remove Columns
  ############################################################
  
  dfImport <- dfImport %>%
    select(
      -any_of(c(
        "raw_ph",
        "raw_ph_qcflag",
        "raw_turbidity",
        "raw_turbidity_qcflag",
        "raw_turbidity_unit",
        "raw_chlorophyll",
        "raw_chlorophyll_unit",
        "raw_chlorophyll_qcflag",
        "raw_orp",
        "raw_orp_unit",
        "raw_orp_qcflag",
        "qaqc_comment",
        "sensorlocation"
      ))
    )
  
  ############################################################
  ### Remove Rows With No Sensor or Unknown Sensor
  ############################################################
  
  dfImport <- dfImport[dfImport$sensortype != "", ]
  dfImport <- dfImport[dfImport$sensortype != "unknown", ]
  
  ############################################################
  ### Make DateTime POSIX
  ############################################################
  
  dateTimeFromTimeUtc <- parseEMPATime(dfImport$time_utc)
  
  badTimeUtc <- is.na(dateTimeFromTimeUtc) |
    as.numeric(format(dateTimeFromTimeUtc, "%Y")) < 1000
  
  dateTimeFromTime <- parseEMPATime(dfImport$time)
  
  dfImport$DateTime <- dateTimeFromTimeUtc
  dfImport$DateTime[badTimeUtc] <- dateTimeFromTime[badTimeUtc]
  
  ############################################################
  ### Add Clustered GPS Fields
  ############################################################
  
  dfImport <- assignGPSByDate(dfImport, dfGPSLookup)
  
  ############################################################
  ### Create GPS Match Summary
  ############################################################
  
  totalRows <- nrow(dfImport)
  gpsMatchedRows <- sum(dfImport$gpsMatchStatus == "matched", na.rm = TRUE)
  gpsUnmatchedRows <- sum(
    dfImport$gpsMatchStatus %in% c(
      "no_sensor_profile_match",
      "no_date_range_match",
      "missing_observation_datetime"
    ),
    na.rm = TRUE
  )
  gpsDuplicateMatchRows <- sum(
    dfImport$gpsMatchStatus == "duplicate_date_range_match",
    na.rm = TRUE
  )
  
  gpsMatchSummaryList[[i]] <- data.frame(
    fileName = i,
    outputName = basename(fullWriteName),
    estuaryFileName = estuaryName,
    totalRows = totalRows,
    gpsMatchedRows = gpsMatchedRows,
    gpsUnmatchedRows = gpsUnmatchedRows,
    gpsDuplicateMatchRows = gpsDuplicateMatchRows,
    noSensorProfileMatchRows = sum(
      dfImport$gpsMatchStatus == "no_sensor_profile_match",
      na.rm = TRUE
    ),
    noDateRangeMatchRows = sum(
      dfImport$gpsMatchStatus == "no_date_range_match",
      na.rm = TRUE
    ),
    missingObservationDateTimeRows = sum(
      dfImport$gpsMatchStatus == "missing_observation_datetime",
      na.rm = TRUE
    ),
    gpsMatchRate = ifelse(totalRows > 0, gpsMatchedRows / totalRows, NA_real_)
  )
  
  ############################################################
  ### Save Cleaned File
  ############################################################
  
  print(range(format(dfImport$DateTime, "%Y"), na.rm = TRUE))
  cat(
    index,
    estuaryName,
    "matched:",
    gpsMatchedRows,
    "unmatched:",
    gpsUnmatchedRows,
    "duplicates:",
    gpsDuplicateMatchRows,
    "\n"
  )
  
  saveRDS(dfImport, fullWriteName)
}

############################################################
### Write GPS Match Summary
############################################################

dfGPSMatchSummary <- bind_rows(gpsMatchSummaryList)
write.csv(dfGPSMatchSummary, strFullGPSMatchSummaryName, row.names = FALSE)

cat("Wrote GPS match summary to:", strFullGPSMatchSummaryName, "\n")
cat("Files processed:", length(fileList), "\n")

############################################################
### Garbage Collector
############################################################

gc()
