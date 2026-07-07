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
  x[x == ""] <- NA_character_
  x[x == "NA"] <- NA_character_
  x[x == "NaN"] <- NA_character_
  x
}

### Parse EMPA time strings as UTC POSIXct.
parseEMPATime <- function(timeValue) {
  timeValue <- blankToNA(timeValue)
  outputTime <- as.POSIXct(rep(NA_character_, length(timeValue)), tz = "UTC")

  idxISO <- !is.na(timeValue) & grepl("T", timeValue)
  outputTime[idxISO] <- as.POSIXct(
    timeValue[idxISO],
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )

  idxSpace <- !is.na(timeValue) & is.na(outputTime)
  outputTime[idxSpace] <- as.POSIXct(
    timeValue[idxSpace],
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )

  idxDateOnly <- !is.na(timeValue) & is.na(outputTime)
  outputTime[idxDateOnly] <- as.POSIXct(
    timeValue[idxDateOnly],
    format = "%Y-%m-%d",
    tz = "UTC"
  )

  outputTime
}

### Stop with a clear message if required fields are missing.
checkRequiredFields <- function(dfInput, requiredFields, dfName) {
  missingFields <- setdiff(requiredFields, names(dfInput))

  if (length(missingFields) > 0) {
    stop(
      dfName,
      " is missing required fields: ",
      paste(missingFields, collapse = ", ")
    )
  }
}

### Add empty GPS output fields to a data frame.
addEmptyGPSFields <- function(dfImport) {
  dfImport$gpsMatchStatus <- NA_character_
  dfImport$gpsMatchCount <- NA_integer_
  dfImport$gpsLatitude <- NA_real_
  dfImport$gpsLongitude <- NA_real_
  dfImport$gpsStartDate <- as.POSIXct(NA, tz = "UTC")
  dfImport$gpsEndDate <- as.POSIXct(NA, tz = "UTC")
  dfImport$gpsUniqueCoordinateEntry <- NA_integer_
  dfImport$gpsOriginalLatitude <- NA_real_
  dfImport$gpsOriginalLongitude <- NA_real_
  dfImport$gpsCombineToSinglePin <- NA_character_
  dfImport$gpsFinalCoordinateGroup <- NA_character_
  dfImport$gpsFinalCoordinateMethod <- NA_character_
  dfImport$gpsFinalPointCount <- NA_integer_
  dfImport$gpsDBSCANCluster <- NA_integer_
  dfImport$gpsDBSCANClusterRadiusMeters <- NA_real_
  dfImport$gpsDBSCANClusterDiameterMeters <- NA_real_
  dfImport$gpsDBSCANClusterPointCount <- NA_real_
  dfImport$gpsStationCoordinateDiameterMeters <- NA_real_
  dfImport$gpsReviewNotes <- NA_character_

  dfImport
}

### Assign one lookup row to matching observations.
assignLookupRow <- function(dfImport, idxAssign, lookupRow) {
  dfImport$gpsLatitude[idxAssign] <- lookupRow$finalLatitude
  dfImport$gpsLongitude[idxAssign] <- lookupRow$finalLongitude
  dfImport$gpsStartDate[idxAssign] <- lookupRow$gpsStartDate
  dfImport$gpsEndDate[idxAssign] <- lookupRow$gpsEndDate
  dfImport$gpsUniqueCoordinateEntry[idxAssign] <- lookupRow$uniqueCoordinateEntry
  dfImport$gpsOriginalLatitude[idxAssign] <- lookupRow$originalLatitude
  dfImport$gpsOriginalLongitude[idxAssign] <- lookupRow$originalLongitude
  dfImport$gpsCombineToSinglePin[idxAssign] <- lookupRow$combineToSinglePin
  dfImport$gpsFinalCoordinateGroup[idxAssign] <- lookupRow$finalCoordinateGroup
  dfImport$gpsFinalCoordinateMethod[idxAssign] <- lookupRow$finalCoordinateMethod
  dfImport$gpsFinalPointCount[idxAssign] <- lookupRow$finalPointCount
  dfImport$gpsDBSCANCluster[idxAssign] <- lookupRow$dbscanCluster
  dfImport$gpsDBSCANClusterRadiusMeters[idxAssign] <-
    lookupRow$dbscanClusterRadiusMeters
  dfImport$gpsDBSCANClusterDiameterMeters[idxAssign] <-
    lookupRow$dbscanClusterDiameterMeters
  dfImport$gpsDBSCANClusterPointCount[idxAssign] <-
    lookupRow$dbscanClusterPointCount
  dfImport$gpsStationCoordinateDiameterMeters[idxAssign] <-
    lookupRow$stationCoordinateDiameterMeters
  dfImport$gpsReviewNotes[idxAssign] <- lookupRow$gpsReviewNotes

  dfImport
}

### Add GPS fields using sensor, profile, station, estuary, and DateTime range.
addGPSFieldsToImport <- function(dfImport, dfGPSLookup) {
  dfImport <- addEmptyGPSFields(dfImport)

  dfImport$gpsMatchCount <- 0L
  dfImport$gpsMatchKey <- paste(
    dfImport$estuaryname,
    dfImport$stationno,
    dfImport$sensorid,
    dfImport$profile,
    sep = "||"
  )

  lookupKeys <- unique(
    paste(
      dfGPSLookup$estuaryname,
      dfGPSLookup$stationno,
      dfGPSLookup$sensorid,
      dfGPSLookup$profile,
      sep = "||"
    )
  )

  dfImport$gpsHasSensorProfileLookup <- dfImport$gpsMatchKey %in% lookupKeys

  for (lookupIndex in seq_len(nrow(dfGPSLookup))) {
    lookupRow <- dfGPSLookup[lookupIndex, ]

    idxMatch <- dfImport$estuaryname == lookupRow$estuaryname &
      dfImport$stationno == lookupRow$stationno &
      dfImport$sensorid == lookupRow$sensorid &
      dfImport$profile == lookupRow$profile &
      !is.na(dfImport$DateTime) &
      !is.na(lookupRow$gpsStartDate) &
      dfImport$DateTime >= lookupRow$gpsStartDate &
      (
        is.na(lookupRow$gpsEndDate) |
          dfImport$DateTime <= lookupRow$gpsEndDate
      )

    idxMatch[is.na(idxMatch)] <- FALSE

    if (!any(idxMatch)) {
      next
    }

    dfImport$gpsMatchCount[idxMatch] <- dfImport$gpsMatchCount[idxMatch] + 1L

    idxFirstMatch <- idxMatch & dfImport$gpsMatchCount == 1L

    if (any(idxFirstMatch)) {
      dfImport <- assignLookupRow(dfImport, idxFirstMatch, lookupRow)
    }
  }

  dfImport$gpsMatchStatus <- case_when(
    is.na(dfImport$DateTime) ~ "missing_observation_datetime",
    dfImport$gpsMatchCount == 1L ~ "matched",
    dfImport$gpsMatchCount > 1L ~ "duplicate_date_range_match",
    !dfImport$gpsHasSensorProfileLookup ~ "no_sensor_profile_match",
    TRUE ~ "no_date_range_match"
  )

  dfImport$gpsMatchKey <- NULL
  dfImport$gpsHasSensorProfileLookup <- NULL

  dfImport
}

############################################################
### Load and Validate GPS Lookup
############################################################

dfGPSLookup <- read.csv(strFullGPSLookupName, stringsAsFactors = FALSE)

requiredGPSLookupFields <- c(
  "estuaryname",
  "stationno",
  "sensorid",
  "profile",
  "gpsStartDate",
  "gpsEndDate",
  "uniqueCoordinateEntry",
  "originalLatitude",
  "originalLongitude",
  "finalLatitude",
  "finalLongitude",
  "combineToSinglePin",
  "finalCoordinateGroup",
  "finalCoordinateMethod",
  "finalPointCount",
  "dbscanCluster",
  "dbscanClusterRadiusMeters",
  "dbscanClusterDiameterMeters",
  "dbscanClusterPointCount",
  "stationCoordinateDiameterMeters",
  "gpsReviewNotes"
)

checkRequiredFields(
  dfGPSLookup,
  requiredGPSLookupFields,
  "empaSensorGPSLookup.csv"
)

dfGPSLookup <- dfGPSLookup %>%
  mutate(
    estuaryname = trimws(as.character(estuaryname)),
    stationno = cleanStationNo(stationno),
    sensorid = trimws(as.character(sensorid)),
    profile = trimws(as.character(profile)),
    gpsStartDate = parseEMPATime(gpsStartDate),
    gpsEndDate = parseEMPATime(gpsEndDate),
    uniqueCoordinateEntry = suppressWarnings(as.integer(uniqueCoordinateEntry)),
    originalLatitude = suppressWarnings(as.numeric(originalLatitude)),
    originalLongitude = suppressWarnings(as.numeric(originalLongitude)),
    finalLatitude = suppressWarnings(as.numeric(finalLatitude)),
    finalLongitude = suppressWarnings(as.numeric(finalLongitude)),
    combineToSinglePin = blankToNA(combineToSinglePin),
    finalCoordinateGroup = blankToNA(finalCoordinateGroup),
    finalCoordinateMethod = blankToNA(finalCoordinateMethod),
    finalPointCount = suppressWarnings(as.integer(finalPointCount)),
    dbscanCluster = suppressWarnings(as.integer(dbscanCluster)),
    dbscanClusterRadiusMeters = suppressWarnings(
      as.numeric(dbscanClusterRadiusMeters)
    ),
    dbscanClusterDiameterMeters = suppressWarnings(
      as.numeric(dbscanClusterDiameterMeters)
    ),
    dbscanClusterPointCount = suppressWarnings(
      as.numeric(dbscanClusterPointCount)
    ),
    stationCoordinateDiameterMeters = suppressWarnings(
      as.numeric(stationCoordinateDiameterMeters)
    ),
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

  dfImport <- addGPSFieldsToImport(dfImport, dfGPSLookup)

  ############################################################
  ### Create GPS Match Summary
  ############################################################

  totalRows <- nrow(dfImport)
  gpsMatchedRows <- sum(dfImport$gpsMatchStatus == "matched", na.rm = TRUE)
  gpsDuplicateMatchRows <- sum(
    dfImport$gpsMatchStatus == "duplicate_date_range_match",
    na.rm = TRUE
  )
  noSensorProfileMatchRows <- sum(
    dfImport$gpsMatchStatus == "no_sensor_profile_match",
    na.rm = TRUE
  )
  noDateRangeMatchRows <- sum(
    dfImport$gpsMatchStatus == "no_date_range_match",
    na.rm = TRUE
  )
  missingObservationDateTimeRows <- sum(
    dfImport$gpsMatchStatus == "missing_observation_datetime",
    na.rm = TRUE
  )
  gpsUnmatchedRows <- noSensorProfileMatchRows +
    noDateRangeMatchRows +
    missingObservationDateTimeRows

  gpsMatchSummaryList[[i]] <- data.frame(
    fileName = i,
    outputName = basename(fullWriteName),
    estuaryFileName = estuaryName,
    totalRows = totalRows,
    gpsMatchedRows = gpsMatchedRows,
    gpsUnmatchedRows = gpsUnmatchedRows,
    gpsDuplicateMatchRows = gpsDuplicateMatchRows,
    noSensorProfileMatchRows = noSensorProfileMatchRows,
    noDateRangeMatchRows = noDateRangeMatchRows,
    missingObservationDateTimeRows = missingObservationDateTimeRows,
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
cat("Total rows processed:", sum(dfGPSMatchSummary$totalRows), "\n")
cat("Total GPS matched rows:", sum(dfGPSMatchSummary$gpsMatchedRows), "\n")
cat("Total GPS unmatched rows:", sum(dfGPSMatchSummary$gpsUnmatchedRows), "\n")
cat("Total GPS duplicate match rows:", sum(dfGPSMatchSummary$gpsDuplicateMatchRows), "\n")

############################################################
### Garbage Collector
############################################################

gc()
