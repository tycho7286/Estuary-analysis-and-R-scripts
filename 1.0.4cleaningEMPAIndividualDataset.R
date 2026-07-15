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
strGPSDuplicateMatchesFilename <- "empaCleaningGPSDuplicateMatches.csv"
strGPSUnmatchedRowsFilename <- "empaCleaningGPSUnmatchedRows.csv"
strGPSStatisticsFilename <- "empaCleaningGPSStatistics.csv"

strFullGPSLookupName <- file.path(strMetadataPath, strGPSLookupFilename)
strFullGPSMatchSummaryName <- file.path(strMetadataPath, strGPSMatchSummaryFilename)
strFullGPSDuplicateMatchesName <- file.path(strMetadataPath, strGPSDuplicateMatchesFilename)
strFullGPSUnmatchedRowsName <- file.path(strMetadataPath, strGPSUnmatchedRowsFilename)
strFullGPSStatisticsName <- file.path(strMetadataPath, strGPSStatisticsFilename)

############################################################
### Script Settings
############################################################

strCleaningVersion <- "1.0"
strCleaningCreatedAt <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

### Set to TRUE to write every unmatched row. This can create a large CSV.
bolWriteUnmatchedRows <- FALSE

### Set to TRUE to write every duplicate match row. This can create a large CSV.
bolWriteDuplicateMatchRows <- FALSE

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

### Collapse unique values into one readable diagnostic string.
collapseUniqueValues <- function(x) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & x != ""]
  paste(x, collapse = " | ")
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
  dfImport$gpsLookupVersion <- NA_character_
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
  dfImport$gpsLookupVersion[idxAssign] <- lookupRow$gpsLookupVersion
  
  dfImport
}

### Build a compact duplicate diagnostic row.
makeDuplicateDiagnostic <- function(dfImport, rowIndex, dfMatches, fileName) {
  data.frame(
    fileName = fileName,
    estuaryname = dfImport$estuaryname[rowIndex],
    stationno = dfImport$stationno[rowIndex],
    sensorid = dfImport$sensorid[rowIndex],
    profile = dfImport$profile[rowIndex],
    DateTime = dfImport$DateTime[rowIndex],
    numberMatches = nrow(dfMatches),
    matchingUniqueCoordinateEntries = collapseUniqueValues(
      dfMatches$uniqueCoordinateEntry
    ),
    matchingCoordinateGroups = collapseUniqueValues(
      dfMatches$finalCoordinateGroup
    ),
    matchingCombineToSinglePins = collapseUniqueValues(
      dfMatches$combineToSinglePin
    ),
    matchingFinalLatitudes = collapseUniqueValues(dfMatches$finalLatitude),
    matchingFinalLongitudes = collapseUniqueValues(dfMatches$finalLongitude),
    matchingStartDates = collapseUniqueValues(dfMatches$gpsStartDate),
    matchingEndDates = collapseUniqueValues(dfMatches$gpsEndDate),
    matchingReviewNotes = collapseUniqueValues(dfMatches$gpsReviewNotes),
    allMatchesSameFinalCoordinate = n_distinct(
      paste(dfMatches$finalLatitude, dfMatches$finalLongitude, sep = ",")
    ) == 1,
    stringsAsFactors = FALSE
  )
}

### Add GPS fields using lookup rows and vectorized DateTime range matching.
addGPSFieldsToImport <- function(dfImport, dfGPSLookup, lookupList, fileName) {
  dfImport <- addEmptyGPSFields(dfImport)
  dfDuplicateDiagnostics <- list()
  
  dfImport$gpsMatchCount <- 0L
  dfImport$gpsMatchKey <- paste(
    dfImport$estuaryname,
    dfImport$stationno,
    dfImport$sensorid,
    dfImport$profile,
    sep = "||"
  )
  
  lookupKeys <- names(lookupList)
  dfImport$gpsHasSensorProfileLookup <- dfImport$gpsMatchKey %in% lookupKeys
  keysInThisFile <- unique(dfImport$gpsMatchKey[dfImport$gpsHasSensorProfileLookup])
  
  dfLookupForFile <- dfGPSLookup[dfGPSLookup$lookupKey %in% keysInThisFile, ]
  
  cat(
    "Lookup rows used for this file:",
    nrow(dfLookupForFile),
    "of",
    nrow(dfGPSLookup),
    "\n"
  )
  
  if (nrow(dfLookupForFile) == 0) {
    dfImport$gpsMatchStatus <- case_when(
      is.na(dfImport$DateTime) ~ "missing_observation_datetime",
      TRUE ~ "no_sensor_profile_match"
    )
    dfImport$gpsMatchKey <- NULL
    dfImport$gpsHasSensorProfileLookup <- NULL
    return(list(
      dfImport = dfImport,
      dfDuplicateDiagnostics = bind_rows(dfDuplicateDiagnostics)
    ))
  }
  
  for (lookupIndex in seq_len(nrow(dfLookupForFile))) {
    lookupRow <- dfLookupForFile[lookupIndex, ]
    
    idxMatch <- dfImport$gpsMatchKey == lookupRow$lookupKey &
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
    
    dfImport$gpsMatchCount[idxMatch] <-
      dfImport$gpsMatchCount[idxMatch] + 1L
    
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
  
  ### Optional duplicate diagnostics. This is intentionally separate and off by default.
  if (bolWriteDuplicateMatchRows && any(dfImport$gpsMatchStatus == "duplicate_date_range_match")) {
    duplicateRows <- which(dfImport$gpsMatchStatus == "duplicate_date_range_match")
    
    for (rowIndex in duplicateRows) {
      currentDateTime <- dfImport$DateTime[rowIndex]
      currentKey <- dfImport$gpsMatchKey[rowIndex]
      dfCandidate <- lookupList[[currentKey]]
      
      idxDateMatch <- !is.na(dfCandidate$gpsStartDate) &
        dfCandidate$gpsStartDate <= currentDateTime &
        (
          is.na(dfCandidate$gpsEndDate) |
            dfCandidate$gpsEndDate >= currentDateTime
        )
      
      idxDateMatch[is.na(idxDateMatch)] <- FALSE
      dfMatches <- dfCandidate[idxDateMatch, ]
      
      dfDuplicateDiagnostics[[length(dfDuplicateDiagnostics) + 1]] <-
        makeDuplicateDiagnostic(dfImport, rowIndex, dfMatches, fileName)
    }
  }
  
  dfImport$gpsMatchKey <- NULL
  dfImport$gpsHasSensorProfileLookup <- NULL
  
  list(
    dfImport = dfImport,
    dfDuplicateDiagnostics = bind_rows(dfDuplicateDiagnostics)
  )
}

############################################################
### Load and Validate GPS Lookup
############################################################

dfGPSLookup <- read.csv(strFullGPSLookupName, stringsAsFactors = FALSE)

requiredGPSLookupFields <- c(
  "gpsLookupVersion",
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
    gpsLookupVersion = trimws(as.character(gpsLookupVersion)),
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
### Build GPS Lookup Index
############################################################

dfGPSLookup$lookupKey <- paste(
  dfGPSLookup$estuaryname,
  dfGPSLookup$stationno,
  dfGPSLookup$sensorid,
  dfGPSLookup$profile,
  sep = "||"
)

lookupList <- split(dfGPSLookup, dfGPSLookup$lookupKey)

############################################################
### Get File Names
############################################################

dir.create(strOutPath, recursive = TRUE, showWarnings = FALSE)

fileList <- list.files(strInPath, pattern = "\\.csv$", ignore.case = TRUE)
gpsMatchSummaryList <- list()
gpsDuplicateDiagnosticsList <- list()
gpsUnmatchedRowsList <- list()
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
  
  cat("Starting GPS match for:", i, "rows:", nrow(dfImport), "\n")
  gpsResult <- addGPSFieldsToImport(dfImport, dfGPSLookup, lookupList, i)
  cat("Finished GPS match for:", i, "\n")
  dfImport <- gpsResult$dfImport
  
  if (nrow(gpsResult$dfDuplicateDiagnostics) > 0) {
    gpsDuplicateDiagnosticsList[[i]] <- gpsResult$dfDuplicateDiagnostics
  }
  
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
    cleaningVersion = strCleaningVersion,
    cleaningCreatedAt = strCleaningCreatedAt,
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
    gpsMatchRate = ifelse(totalRows > 0, gpsMatchedRows / totalRows, NA_real_),
    stringsAsFactors = FALSE
  )
  
  ############################################################
  ### Store Unmatched Row Diagnostics
  ############################################################
  
  if (bolWriteUnmatchedRows) {
    dfUnmatched <- dfImport %>%
      filter(
        gpsMatchStatus %in% c(
          "no_sensor_profile_match",
          "no_date_range_match",
          "missing_observation_datetime"
        )
      ) %>%
      select(
        estuaryname,
        stationno,
        sensorid,
        profile,
        DateTime,
        gpsMatchStatus,
        gpsMatchCount,
        everything()
      ) %>%
      mutate(sourceFileName = i, .before = estuaryname)
    
    if (nrow(dfUnmatched) > 0) {
      gpsUnmatchedRowsList[[i]] <- dfUnmatched
    }
  }
  
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
  
  cat("Saving file to:", fullWriteName, "\n")
  saveRDS(dfImport, fullWriteName)
  cat("Finished saving:", fullWriteName, "\n")
}


############################################################
### Write GPS Diagnostics
############################################################

dfGPSMatchSummary <- bind_rows(gpsMatchSummaryList)
dfGPSDuplicateMatches <- bind_rows(gpsDuplicateDiagnosticsList)
dfGPSUnmatchedRows <- bind_rows(gpsUnmatchedRowsList)

write.csv(dfGPSMatchSummary, strFullGPSMatchSummaryName, row.names = FALSE)
write.csv(dfGPSDuplicateMatches, strFullGPSDuplicateMatchesName, row.names = FALSE)
write.csv(dfGPSUnmatchedRows, strFullGPSUnmatchedRowsName, row.names = FALSE)

dfGPSStatistics <- data.frame(
  cleaningVersion = strCleaningVersion,
  cleaningCreatedAt = strCleaningCreatedAt,
  filesProcessed = length(fileList),
  lookupRows = nrow(dfGPSLookup),
  lookupKeys = length(lookupList),
  totalRowsProcessed = sum(dfGPSMatchSummary$totalRows),
  totalGPSMatchedRows = sum(dfGPSMatchSummary$gpsMatchedRows),
  totalGPSUnmatchedRows = sum(dfGPSMatchSummary$gpsUnmatchedRows),
  totalGPSDuplicateMatchRows = sum(dfGPSMatchSummary$gpsDuplicateMatchRows),
  totalNoSensorProfileMatchRows = sum(dfGPSMatchSummary$noSensorProfileMatchRows),
  totalNoDateRangeMatchRows = sum(dfGPSMatchSummary$noDateRangeMatchRows),
  totalMissingObservationDateTimeRows = sum(
    dfGPSMatchSummary$missingObservationDateTimeRows
  ),
  overallGPSMatchRate = sum(dfGPSMatchSummary$gpsMatchedRows) /
    sum(dfGPSMatchSummary$totalRows),
  stringsAsFactors = FALSE
)

write.csv(dfGPSStatistics, strFullGPSStatisticsName, row.names = FALSE)

cat("Wrote GPS match summary to:", strFullGPSMatchSummaryName, "\n")
cat("Wrote GPS duplicate match details to:", strFullGPSDuplicateMatchesName, "\n")
cat("Wrote GPS unmatched row details to:", strFullGPSUnmatchedRowsName, "\n")
cat("Wrote GPS statistics to:", strFullGPSStatisticsName, "\n")
cat("Files processed:", length(fileList), "\n")
cat("Total rows processed:", dfGPSStatistics$totalRowsProcessed, "\n")
cat("Total GPS matched rows:", dfGPSStatistics$totalGPSMatchedRows, "\n")
cat("Total GPS unmatched rows:", dfGPSStatistics$totalGPSUnmatchedRows, "\n")
cat("Total GPS duplicate match rows:", dfGPSStatistics$totalGPSDuplicateMatchRows, "\n")

############################################################
### Garbage Collector
############################################################

gc()
