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

strFullMembershipName <- file.path(strInPath, strMembershipFilename)
strFullClusterReviewName <- file.path(strInPath, strClusterReviewFilename)
strFullLookupWriteName <- file.path(strOutPath, strLookupWriteFilename)

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

############################################################
### Load Files
############################################################

dfMembership <- read.csv(strFullMembershipName, stringsAsFactors = FALSE)
dfClusters <- read.csv(strFullClusterReviewName, stringsAsFactors = FALSE)

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
    gpsStartDate = as.POSIXct(
      time,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    gpsEndDate = as.POSIXct(
      time_end,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
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
    dbscanClusterCenterLatitude = suppressWarnings(
      as.numeric(dbscanClusterCenterLatitude)
    ),
    dbscanClusterCenterLongitude = suppressWarnings(
      as.numeric(dbscanClusterCenterLongitude)
    ),
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
    )
  )

############################################################
### Add Manual Review Fields if Missing
############################################################

if (!"combineToSinglePin" %in% names(dfClusters)) {
  dfClusters$combineToSinglePin <- NA_character_
}

if (!"gpsUse" %in% names(dfClusters)) {
  dfClusters$gpsUse <- TRUE
}

if (!"gpsReviewNotes" %in% names(dfClusters)) {
  dfClusters$gpsReviewNotes <- NA_character_
}

if (!"manualLatitude" %in% names(dfClusters)) {
  dfClusters$manualLatitude <- NA_real_
}

if (!"manualLongitude" %in% names(dfClusters)) {
  dfClusters$manualLongitude <- NA_real_
}

############################################################
### Clean Manual Review Fields
############################################################

dfClusters <- dfClusters %>%
  mutate(
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
    finalCoordinateGroup,
    finalCoordinateMethod,
    finalPointCount,
    combineToSinglePin,
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
    finalCoordinateGroup,
    finalCoordinateMethod,
    finalPointCount,
    combineToSinglePin,
    dbscanCluster,
    dbscanClusterRadiusMeters,
    dbscanClusterDiameterMeters,
    dbscanClusterPointCount,
    stationCoordinateDiameterMeters,
    gpsReviewNotes,
    everything()
  ) %>%
  arrange(
    estuaryname,
    suppressWarnings(as.numeric(stationno)),
    stationno,
    sensorid,
    profile,
    gpsStartDate
  )

############################################################
### Check for Possible Duplicate Lookup Rows
############################################################

dfDuplicateCheck <- dfSensorGPSLookup %>%
  group_by(estuaryname, stationno, sensorid, profile, gpsStartDate, gpsEndDate) %>%
  summarise(
    lookupRows = n(),
    .groups = "drop"
  ) %>%
  filter(lookupRows > 1)

if (nrow(dfDuplicateCheck) > 0) {
  warning(
    "Some sensor date profile records have more than one GPS lookup row. ",
    "Review duplicate lookup records before using the lookup table."
  )
}

############################################################
### Write Lookup Table
############################################################

write.csv(dfSensorGPSLookup, strFullLookupWriteName, row.names = FALSE)

cat("Wrote GPS lookup table to:", strFullLookupWriteName, "\n")
cat("Rows written:", nrow(dfSensorGPSLookup), "\n")
cat("Duplicate lookup checks found:", nrow(dfDuplicateCheck), "\n")

if (nrow(dfDuplicateCheck) > 0) {
  print(dfDuplicateCheck)
}

gc()
