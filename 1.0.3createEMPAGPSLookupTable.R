############################################################
### Install and Load Packages
############################################################

# install.packages("dplyr")
library(dplyr)

############################################################
### File Paths
############################################################

strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/metadata"
strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/metadata"

strMembershipFilename <- "empaStationCoordinateMembership.csv"
strClusterReviewFilename <- "empaStationCoordinateClustersEdited.csv"
strLookupWriteFilename <- "empaSensorGPSLookup.csv"
strDuplicateWriteFilename <- "empaSensorGPSLookupDuplicateDateRanges.csv"

strFullMembershipName <- file.path(strInPath, strMembershipFilename)
strFullClusterReviewName <- file.path(strInPath, strClusterReviewFilename)
strFullLookupWriteName <- file.path(strOutPath, strLookupWriteFilename)
strFullDuplicateWriteName <- file.path(strOutPath, strDuplicateWriteFilename)

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

### Convert text values to TRUE or FALSE.
cleanLogicalField <- function(x, defaultValue = TRUE) {
  x <- as.character(x)
  x <- trimws(tolower(x))

  output <- rep(defaultValue, length(x))
  output[x %in% c("true", "t", "yes", "y", "1")] <- TRUE
  output[x %in% c("false", "f", "no", "n", "0")] <- FALSE

  output
}

### Parse EMPA ISO time strings as UTC POSIXct.
parseEMPATime <- function(timeValue) {
  timeValue <- blankToNA(timeValue)

  parsedTime <- as.POSIXct(
    timeValue,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )

  parsedTime
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

### Add a column if it is missing.
addMissingColumn <- function(dfInput, columnName, defaultValue) {
  if (!columnName %in% names(dfInput)) {
    dfInput[[columnName]] <- defaultValue
  }

  dfInput
}

############################################################
### Load Files
############################################################

dfMembership <- read.csv(strFullMembershipName, stringsAsFactors = FALSE)
dfClusters <- read.csv(strFullClusterReviewName, stringsAsFactors = FALSE)

############################################################
### Check Required Input Fields
############################################################

requiredMembershipFields <- c(
  "estuaryname",
  "stationno",
  "sensorid",
  "profile",
  "time",
  "time_end",
  "uniqueCoordinateEntry"
)

requiredClusterFields <- c(
  "estuaryname",
  "stationno",
  "uniqueCoordinateEntry",
  "latitude",
  "longitude",
  "dbscanCluster",
  "dbscanClusterRadiusMeters",
  "dbscanClusterDiameterMeters",
  "dbscanClusterPointCount",
  "stationCoordinateDiameterMeters"
)

checkRequiredFields(
  dfMembership,
  requiredMembershipFields,
  "empaStationCoordinateMembership.csv"
)

checkRequiredFields(
  dfClusters,
  requiredClusterFields,
  "empaStationCoordinateClustersEdited.csv"
)

############################################################
### Add Optional Manual Review Fields if Missing
############################################################

dfClusters <- addMissingColumn(dfClusters, "combineToSinglePin", NA_character_)
dfClusters <- addMissingColumn(dfClusters, "gpsUse", TRUE)
dfClusters <- addMissingColumn(dfClusters, "gpsReviewNotes", NA_character_)
dfClusters <- addMissingColumn(dfClusters, "manualLatitude", NA_real_)
dfClusters <- addMissingColumn(dfClusters, "manualLongitude", NA_real_)

############################################################
### Clean Membership Fields
############################################################

dfMembership <- dfMembership %>%
  mutate(
    estuaryname = trimws(as.character(estuaryname)),
    stationno = cleanStationNo(stationno),
    sensorid = trimws(as.character(sensorid)),
    profile = trimws(as.character(profile)),
    uniqueCoordinateEntry = suppressWarnings(as.integer(uniqueCoordinateEntry)),
    gpsStartDate = parseEMPATime(time),
    gpsEndDate = parseEMPATime(time_end)
  )

############################################################
### Clean Cluster Review Fields
############################################################

dfClusters <- dfClusters %>%
  mutate(
    estuaryname = trimws(as.character(estuaryname)),
    stationno = cleanStationNo(stationno),
    uniqueCoordinateEntry = suppressWarnings(as.integer(uniqueCoordinateEntry)),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude)),
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
    combineToSinglePin = blankToNA(combineToSinglePin),
    gpsUse = cleanLogicalField(gpsUse, defaultValue = TRUE),
    gpsReviewNotes = blankToNA(gpsReviewNotes),
    manualLatitude = suppressWarnings(as.numeric(manualLatitude)),
    manualLongitude = suppressWarnings(as.numeric(manualLongitude))
  )

############################################################
### Create Final Reviewed Coordinate Groups
############################################################

dfClusters <- dfClusters %>%
  mutate(
    originalLatitude = latitude,
    originalLongitude = longitude,
    finalCoordinateGroup = ifelse(
      is.na(combineToSinglePin),
      paste0("entry_", uniqueCoordinateEntry),
      paste0("combined_", combineToSinglePin)
    )
  )

dfFinalCoordinateCenters <- dfClusters %>%
  filter(gpsUse) %>%
  group_by(estuaryname, stationno, finalCoordinateGroup) %>%
  summarise(
    combinedCenterLatitude = mean(originalLatitude, na.rm = TRUE),
    combinedCenterLongitude = mean(originalLongitude, na.rm = TRUE),
    finalPointCount = n(),
    .groups = "drop"
  )

dfClusters <- dfClusters %>%
  left_join(
    dfFinalCoordinateCenters,
    by = c("estuaryname", "stationno", "finalCoordinateGroup")
  ) %>%
  mutate(
    finalLatitude = case_when(
      !is.na(manualLatitude) ~ manualLatitude,
      !is.na(combineToSinglePin) ~ combinedCenterLatitude,
      TRUE ~ originalLatitude
    ),
    finalLongitude = case_when(
      !is.na(manualLongitude) ~ manualLongitude,
      !is.na(combineToSinglePin) ~ combinedCenterLongitude,
      TRUE ~ originalLongitude
    ),
    finalCoordinateMethod = case_when(
      !is.na(manualLatitude) & !is.na(manualLongitude) ~ "Manual Coordinate",
      !is.na(combineToSinglePin) ~ "Combined Coordinate Average",
      TRUE ~ "Original Coordinate"
    )
  )

############################################################
### Create Coordinate Lookup From Reviewed Clusters
############################################################

dfCoordinateLookup <- dfClusters %>%
  filter(gpsUse) %>%
  filter(!is.na(finalLatitude), !is.na(finalLongitude)) %>%
  select(
    estuaryname,
    stationno,
    uniqueCoordinateEntry,
    originalLatitude,
    originalLongitude,
    finalLatitude,
    finalLongitude,
    combineToSinglePin,
    finalCoordinateGroup,
    finalCoordinateMethod,
    finalPointCount,
    dbscanCluster,
    dbscanClusterRadiusMeters,
    dbscanClusterDiameterMeters,
    dbscanClusterPointCount,
    stationCoordinateDiameterMeters,
    gpsReviewNotes
  )

############################################################
### Join Coordinate Lookup to Full Sensor Membership
############################################################

dfSensorGPSLookup <- dfMembership %>%
  left_join(
    dfCoordinateLookup,
    by = c("estuaryname", "stationno", "uniqueCoordinateEntry")
  ) %>%
  filter(!is.na(finalLatitude), !is.na(finalLongitude)) %>%
  select(
    estuaryname,
    stationno,
    sensorid,
    profile,
    gpsStartDate,
    gpsEndDate,
    uniqueCoordinateEntry,
    originalLatitude,
    originalLongitude,
    finalLatitude,
    finalLongitude,
    combineToSinglePin,
    finalCoordinateGroup,
    finalCoordinateMethod,
    finalPointCount,
    dbscanCluster,
    dbscanClusterRadiusMeters,
    dbscanClusterDiameterMeters,
    dbscanClusterPointCount,
    stationCoordinateDiameterMeters,
    gpsReviewNotes
  ) %>%
  arrange(
    estuaryname,
    suppressWarnings(as.numeric(stationno)),
    stationno,
    sensorid,
    profile,
    gpsStartDate,
    gpsEndDate,
    uniqueCoordinateEntry
  )

############################################################
### Check Final Lookup Structure
############################################################

finalLookupFields <- c(
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
  dfSensorGPSLookup,
  finalLookupFields,
  "empaSensorGPSLookup.csv"
)

dfSensorGPSLookup <- dfSensorGPSLookup[, finalLookupFields]

############################################################
### Check for Overlapping Deployment Date Ranges
############################################################

dfOverlapList <- list()
lookupGroups <- dfSensorGPSLookup %>%
  distinct(estuaryname, stationno, sensorid, profile)

for (i in seq_len(nrow(lookupGroups))) {
  currentGroup <- lookupGroups[i, ]

  dfGroup <- dfSensorGPSLookup %>%
    filter(
      estuaryname == currentGroup$estuaryname,
      stationno == currentGroup$stationno,
      sensorid == currentGroup$sensorid,
      profile == currentGroup$profile
    ) %>%
    arrange(gpsStartDate, gpsEndDate, uniqueCoordinateEntry)

  if (nrow(dfGroup) <= 1) {
    next
  }

  for (j in seq_len(nrow(dfGroup) - 1)) {
    for (k in (j + 1):nrow(dfGroup)) {
      startOne <- dfGroup$gpsStartDate[j]
      startTwo <- dfGroup$gpsStartDate[k]
      endOne <- dfGroup$gpsEndDate[j]
      endTwo <- dfGroup$gpsEndDate[k]

      if (is.na(startOne) || is.na(startTwo)) {
        next
      }

      if (is.na(endOne)) {
        endOne <- as.POSIXct("9999-12-31", tz = "UTC")
      }

      if (is.na(endTwo)) {
        endTwo <- as.POSIXct("9999-12-31", tz = "UTC")
      }

      rangesOverlap <- startOne <= endTwo & startTwo <= endOne

      if (rangesOverlap) {
        dfOverlapList[[length(dfOverlapList) + 1]] <- data.frame(
          estuaryname = currentGroup$estuaryname,
          stationno = currentGroup$stationno,
          sensorid = currentGroup$sensorid,
          profile = currentGroup$profile,
          firstUniqueCoordinateEntry = dfGroup$uniqueCoordinateEntry[j],
          secondUniqueCoordinateEntry = dfGroup$uniqueCoordinateEntry[k],
          firstStartDate = dfGroup$gpsStartDate[j],
          firstEndDate = dfGroup$gpsEndDate[j],
          secondStartDate = dfGroup$gpsStartDate[k],
          secondEndDate = dfGroup$gpsEndDate[k],
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

dfOverlapCheck <- bind_rows(dfOverlapList)

############################################################
### Write Lookup and Diagnostics
############################################################

write.csv(dfSensorGPSLookup, strFullLookupWriteName, row.names = FALSE)
write.csv(dfOverlapCheck, strFullDuplicateWriteName, row.names = FALSE)

cat("Wrote GPS lookup table to:", strFullLookupWriteName, "\n")
cat("Rows written:", nrow(dfSensorGPSLookup), "\n")
cat("Wrote duplicate date range diagnostic to:", strFullDuplicateWriteName, "\n")
cat("Overlapping date range pairs found:", nrow(dfOverlapCheck), "\n")

if (nrow(dfOverlapCheck) > 0) {
  print(dfOverlapCheck)
}

gc()
