############################################################
### Install and Load Packages
############################################################

# install.packages("dplyr")
library(dplyr)

############################################################
### File Paths
############################################################

strMetadataPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/metadata"

strMembershipFilename <- "empaStationCoordinateMembership.csv"
strCoordinateEntriesFilename <- "empaStationUniqueCoordinateEntries.csv"
strClusterReviewFilename <- "empaStationCoordinateClustersEdited.csv"
strLookupFilename <- "empaSensorGPSLookup.csv"

strValidationSummaryFilename <- "empaMetadataValidationSummary.csv"
strValidationDetailFilename <- "empaMetadataValidationDetails.csv"

strFullMembershipName <- file.path(strMetadataPath, strMembershipFilename)
strFullCoordinateEntriesName <- file.path(strMetadataPath, strCoordinateEntriesFilename)
strFullClusterReviewName <- file.path(strMetadataPath, strClusterReviewFilename)
strFullLookupName <- file.path(strMetadataPath, strLookupFilename)
strFullValidationSummaryName <- file.path(strMetadataPath, strValidationSummaryFilename)
strFullValidationDetailName <- file.path(strMetadataPath, strValidationDetailFilename)

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

### Add one validation result row.
addValidationResult <- function(summaryList, checkName, status, count, notes) {
  summaryList[[length(summaryList) + 1]] <- data.frame(
    checkName = checkName,
    status = status,
    count = count,
    notes = notes,
    stringsAsFactors = FALSE
  )
  summaryList
}

### Read a CSV if it exists.
readCSVIfExists <- function(filePath) {
  if (!file.exists(filePath)) {
    return(NULL)
  }
  read.csv(filePath, stringsAsFactors = FALSE)
}

############################################################
### Load Files
############################################################

dfMembership <- readCSVIfExists(strFullMembershipName)
dfCoordinateEntries <- readCSVIfExists(strFullCoordinateEntriesName)
dfClusters <- readCSVIfExists(strFullClusterReviewName)
dfLookup <- readCSVIfExists(strFullLookupName)

summaryList <- list()
detailList <- list()

############################################################
### Validate Required Files
############################################################

requiredFiles <- data.frame(
  fileLabel = c(
    "Membership",
    "Coordinate Entries",
    "Cluster Review"
  ),
  filePath = c(
    strFullMembershipName,
    strFullCoordinateEntriesName,
    strFullClusterReviewName
  ),
  stringsAsFactors = FALSE
)

missingFiles <- requiredFiles[!file.exists(requiredFiles$filePath), ]

summaryList <- addValidationResult(
  summaryList,
  "required_files_exist",
  ifelse(nrow(missingFiles) == 0, "PASS", "FAIL"),
  nrow(missingFiles),
  "Required files needed before creating the GPS lookup table."
)

if (nrow(missingFiles) > 0) {
  detailList[["missingFiles"]] <- missingFiles
}

if (is.null(dfMembership) || is.null(dfCoordinateEntries) || is.null(dfClusters)) {
  dfValidationSummary <- bind_rows(summaryList)
  dfValidationDetails <- bind_rows(detailList, .id = "detailType")
  write.csv(dfValidationSummary, strFullValidationSummaryName, row.names = FALSE)
  write.csv(dfValidationDetails, strFullValidationDetailName, row.names = FALSE)
  stop("Missing required metadata files. Validation stopped.")
}

############################################################
### Clean Core Fields
############################################################

dfMembership <- dfMembership %>%
  mutate(
    estuaryname = trimws(as.character(estuaryname)),
    stationno = cleanStationNo(stationno),
    sensorid = trimws(as.character(sensorid)),
    profile = trimws(as.character(profile)),
    uniqueCoordinateEntry = suppressWarnings(as.integer(uniqueCoordinateEntry)),
    gpsStartDate = parseEMPATime(time),
    gpsEndDate = parseEMPATime(time_end),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude))
  )

dfCoordinateEntries <- dfCoordinateEntries %>%
  mutate(
    estuaryname = trimws(as.character(estuaryname)),
    stationno = cleanStationNo(stationno),
    uniqueCoordinateEntry = suppressWarnings(as.integer(uniqueCoordinateEntry)),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude))
  )

dfClusters <- dfClusters %>%
  mutate(
    estuaryname = trimws(as.character(estuaryname)),
    stationno = cleanStationNo(stationno),
    uniqueCoordinateEntry = suppressWarnings(as.integer(uniqueCoordinateEntry)),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude)),
    dbscanCluster = suppressWarnings(as.integer(dbscanCluster))
  )

############################################################
### Validate Missing Core Values
############################################################

missingMembershipCore <- dfMembership %>%
  filter(
    is.na(estuaryname) | estuaryname == "" |
      is.na(stationno) | stationno == "" |
      is.na(sensorid) | sensorid == "" |
      is.na(profile) | profile == "" |
      is.na(uniqueCoordinateEntry)
  )

summaryList <- addValidationResult(
  summaryList,
  "membership_missing_core_fields",
  ifelse(nrow(missingMembershipCore) == 0, "PASS", "WARNING"),
  nrow(missingMembershipCore),
  "Membership rows missing estuary, station, sensor, profile, or coordinate entry."
)

if (nrow(missingMembershipCore) > 0) {
  detailList[["membershipMissingCore"]] <- missingMembershipCore
}

missingCoordinateEntries <- dfCoordinateEntries %>%
  filter(is.na(latitude) | is.na(longitude) | is.na(uniqueCoordinateEntry))

summaryList <- addValidationResult(
  summaryList,
  "coordinate_entries_missing_gps",
  ifelse(nrow(missingCoordinateEntries) == 0, "PASS", "WARNING"),
  nrow(missingCoordinateEntries),
  "Coordinate entry rows missing latitude, longitude, or uniqueCoordinateEntry."
)

if (nrow(missingCoordinateEntries) > 0) {
  detailList[["coordinateEntriesMissingGPS"]] <- missingCoordinateEntries
}

############################################################
### Validate Coordinate Entry Keys
############################################################

duplicateCoordinateEntryKeys <- dfCoordinateEntries %>%
  group_by(estuaryname, stationno, uniqueCoordinateEntry) %>%
  summarise(rowCount = n(), .groups = "drop") %>%
  filter(rowCount > 1)

summaryList <- addValidationResult(
  summaryList,
  "duplicate_coordinate_entry_keys",
  ifelse(nrow(duplicateCoordinateEntryKeys) == 0, "PASS", "FAIL"),
  nrow(duplicateCoordinateEntryKeys),
  "Each estuary, station, uniqueCoordinateEntry should appear once in the coordinate entry file."
)

if (nrow(duplicateCoordinateEntryKeys) > 0) {
  detailList[["duplicateCoordinateEntryKeys"]] <- duplicateCoordinateEntryKeys
}

membershipMissingCoordinateEntry <- dfMembership %>%
  anti_join(
    dfCoordinateEntries %>%
      select(estuaryname, stationno, uniqueCoordinateEntry),
    by = c("estuaryname", "stationno", "uniqueCoordinateEntry")
  )

summaryList <- addValidationResult(
  summaryList,
  "membership_references_missing_coordinate_entry",
  ifelse(nrow(membershipMissingCoordinateEntry) == 0, "PASS", "FAIL"),
  nrow(membershipMissingCoordinateEntry),
  "Membership rows whose uniqueCoordinateEntry does not exist in the coordinate entry file."
)

if (nrow(membershipMissingCoordinateEntry) > 0) {
  detailList[["membershipMissingCoordinateEntry"]] <- membershipMissingCoordinateEntry
}

unusedCoordinateEntries <- dfCoordinateEntries %>%
  anti_join(
    dfMembership %>%
      select(estuaryname, stationno, uniqueCoordinateEntry),
    by = c("estuaryname", "stationno", "uniqueCoordinateEntry")
  )

summaryList <- addValidationResult(
  summaryList,
  "unused_coordinate_entries",
  ifelse(nrow(unusedCoordinateEntries) == 0, "PASS", "WARNING"),
  nrow(unusedCoordinateEntries),
  "Coordinate entries that are not used by any membership row."
)

if (nrow(unusedCoordinateEntries) > 0) {
  detailList[["unusedCoordinateEntries"]] <- unusedCoordinateEntries
}

############################################################
### Validate Date Ranges
############################################################

badDateRanges <- dfMembership %>%
  filter(
    is.na(gpsStartDate) |
      (!is.na(gpsEndDate) & gpsEndDate < gpsStartDate)
  )

summaryList <- addValidationResult(
  summaryList,
  "bad_or_missing_date_ranges",
  ifelse(nrow(badDateRanges) == 0, "PASS", "FAIL"),
  nrow(badDateRanges),
  "Membership rows with missing start dates or end dates before start dates."
)

if (nrow(badDateRanges) > 0) {
  detailList[["badDateRanges"]] <- badDateRanges
}

############################################################
### Detect Overlapping Deployments
############################################################

lookupGroups <- dfMembership %>%
  filter(!is.na(gpsStartDate)) %>%
  distinct(estuaryname, stationno, sensorid, profile)

overlapList <- list()

for (i in seq_len(nrow(lookupGroups))) {
  currentGroup <- lookupGroups[i, ]
  
  dfGroup <- dfMembership %>%
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
      
      if (startOne <= endTwo & startTwo <= endOne) {
        overlapStart <- max(startOne, startTwo)
        overlapEnd <- min(endOne, endTwo)
        overlapDays <- as.numeric(difftime(overlapEnd, overlapStart, units = "days"))
        
        overlapList[[length(overlapList) + 1]] <- data.frame(
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
          overlapDays = overlapDays,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

dfOverlaps <- bind_rows(overlapList)

summaryList <- addValidationResult(
  summaryList,
  "overlapping_deployment_date_ranges",
  ifelse(nrow(dfOverlaps) == 0, "PASS", "WARNING"),
  nrow(dfOverlaps),
  "Pairs of deployments for the same estuary, station, sensor, and profile with overlapping dates."
)

if (nrow(dfOverlaps) > 0) {
  detailList[["overlappingDeployments"]] <- dfOverlaps
}

############################################################
### Validate Cluster Review
############################################################

clustersMissingEntries <- dfClusters %>%
  anti_join(
    dfCoordinateEntries %>%
      select(estuaryname, stationno, uniqueCoordinateEntry),
    by = c("estuaryname", "stationno", "uniqueCoordinateEntry")
  )

summaryList <- addValidationResult(
  summaryList,
  "cluster_review_references_missing_coordinate_entry",
  ifelse(nrow(clustersMissingEntries) == 0, "PASS", "FAIL"),
  nrow(clustersMissingEntries),
  "Cluster review rows whose coordinate entry is not present in the coordinate entry file."
)

if (nrow(clustersMissingEntries) > 0) {
  detailList[["clustersMissingEntries"]] <- clustersMissingEntries
}

largeClusterStations <- dfClusters %>%
  group_by(estuaryname, stationno) %>%
  summarise(
    maxStationCoordinateDiameterMeters = max(
      stationCoordinateDiameterMeters,
      na.rm = TRUE
    ),
    coordinateEntries = n(),
    .groups = "drop"
  ) %>%
  filter(maxStationCoordinateDiameterMeters > 100 | coordinateEntries > 10)

summaryList <- addValidationResult(
  summaryList,
  "large_or_complex_station_coordinate_sets",
  ifelse(nrow(largeClusterStations) == 0, "PASS", "WARNING"),
  nrow(largeClusterStations),
  "Stations with coordinate diameters over 100 meters or more than 10 coordinate entries."
)

if (nrow(largeClusterStations) > 0) {
  detailList[["largeClusterStations"]] <- largeClusterStations
}

############################################################
### Write Validation Outputs
############################################################

dfValidationSummary <- bind_rows(summaryList)
dfValidationDetails <- bind_rows(detailList, .id = "detailType")

write.csv(dfValidationSummary, strFullValidationSummaryName, row.names = FALSE)
write.csv(dfValidationDetails, strFullValidationDetailName, row.names = FALSE)

cat("Wrote validation summary to:", strFullValidationSummaryName, "\n")
cat("Wrote validation details to:", strFullValidationDetailName, "\n")
cat("Validation checks completed:", nrow(dfValidationSummary), "\n")
print(dfValidationSummary)

gc()
