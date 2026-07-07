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

strReadFilename <- "empa_logger_meta_9cd4_881c_2f2a.csv"

strUniqueCoordinateWriteFilename <- "empaStationUniqueCoordinateEntries.csv"
strMembershipWriteFilename <- "empaStationCoordinateMembership.csv"

strFullReadName <- file.path(strInPath, strReadFilename)
strFullUniqueCoordinateWriteName <- file.path(strOutPath, strUniqueCoordinateWriteFilename)
strFullMembershipWriteName <- file.path(strOutPath, strMembershipWriteFilename)

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

### Calculate distance in meters between two latitude longitude points.
calculateDistanceMeters <- function(latOne, lonOne, latTwo, lonTwo) {
  earthRadiusMeters <- 6371000
  
  latOneRad <- latOne * pi / 180
  lonOneRad <- lonOne * pi / 180
  latTwoRad <- latTwo * pi / 180
  lonTwoRad <- lonTwo * pi / 180
  
  deltaLat <- latTwoRad - latOneRad
  deltaLon <- lonTwoRad - lonOneRad
  
  a <- sin(deltaLat / 2)^2 +
    cos(latOneRad) * cos(latTwoRad) *
    sin(deltaLon / 2)^2
  
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  earthRadiusMeters * c
}

############################################################
### Load Metadata
############################################################

dfMetadata <- read.csv(strFullReadName, stringsAsFactors = FALSE)

############################################################
### Clean Metadata Fields
############################################################

dfMetadata <- dfMetadata %>%
  mutate(
    estuaryname = trimws(as.character(estuaryname)),
    stationno = cleanStationNo(stationno),
    profile = trimws(as.character(profile)),
    sensorid = trimws(as.character(sensorid)),
    sensortype = trimws(as.character(sensortype)),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude)),
    coordinateStartDate = as.POSIXct(
      time,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    coordinateEndDate = as.POSIXct(
      time_end,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
  )

############################################################
### Keep Valid Coordinate Metadata
############################################################

dfValidMetadata <- dfMetadata %>%
  filter(
    !is.na(estuaryname),
    estuaryname != "",
    !is.na(stationno),
    stationno != "",
    !is.na(latitude),
    !is.na(longitude)
  )

############################################################
### Create Unique Coordinate Entries
############################################################

dfUniqueCoordinates <- dfValidMetadata %>%
  distinct(
    estuaryname,
    stationno,
    latitude,
    longitude
  ) %>%
  group_by(estuaryname, stationno) %>%
  arrange(latitude, longitude, .by_group = TRUE) %>%
  mutate(
    uniqueCoordinateEntry = row_number(),
    numberUniqueCoordinateEntries = n(),
    stationCenterLatitude = mean(latitude, na.rm = TRUE),
    stationCenterLongitude = mean(longitude, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    distanceFromStationCenterMeters = calculateDistanceMeters(
      latitude,
      longitude,
      stationCenterLatitude,
      stationCenterLongitude
    )
  ) %>%
  group_by(estuaryname, stationno) %>%
  mutate(
    maxDistanceFromStationCenterMeters = max(
      distanceFromStationCenterMeters,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  arrange(
    desc(maxDistanceFromStationCenterMeters),
    estuaryname,
    suppressWarnings(as.numeric(stationno)),
    stationno,
    uniqueCoordinateEntry
  )

############################################################
### Create Coordinate Membership Table
############################################################

dfCoordinateMembership <- dfValidMetadata %>%
  left_join(
    dfUniqueCoordinates %>%
      select(
        estuaryname,
        stationno,
        latitude,
        longitude,
        uniqueCoordinateEntry,
        numberUniqueCoordinateEntries,
        stationCenterLatitude,
        stationCenterLongitude,
        distanceFromStationCenterMeters,
        maxDistanceFromStationCenterMeters
      ),
    by = c("estuaryname", "stationno", "latitude", "longitude")
  ) %>%
  arrange(
    estuaryname,
    suppressWarnings(as.numeric(stationno)),
    stationno,
    uniqueCoordinateEntry,
    profile,
    sensorid,
    coordinateStartDate
  )

############################################################
### Write CSV Files
############################################################

write.csv(dfUniqueCoordinates, strFullUniqueCoordinateWriteName, row.names = FALSE)
write.csv(dfCoordinateMembership, strFullMembershipWriteName, row.names = FALSE)

cat("Wrote unique coordinate file to:", strFullUniqueCoordinateWriteName, "\n")
cat("Unique coordinate rows written:", nrow(dfUniqueCoordinates), "\n\n")

cat("Wrote coordinate membership file to:", strFullMembershipWriteName, "\n")
cat("Membership rows written:", nrow(dfCoordinateMembership), "\n")

gc()