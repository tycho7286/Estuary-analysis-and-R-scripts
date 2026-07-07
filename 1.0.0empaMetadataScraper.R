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

strReadFilename <- "metadataEMPA.csv"
strWriteFilename <- "empaStationUniqueCoordinateEntries.csv"

strFullReadName <- file.path(strInPath, strReadFilename)
strFullWriteName <- file.path(strOutPath, strWriteFilename)

############################################################
### Helper Functions
############################################################

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
### Load Coordinate Summary
############################################################

dfCoordinates <- read.csv(strFullReadName, stringsAsFactors = FALSE)

dfCoordinates <- dfCoordinates %>%
  mutate(
    estuaryname = trimws(as.character(estuaryname)),
    stationno = trimws(as.character(stationno)),
    profile = trimws(as.character(profile)),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude))
  )

############################################################
### Split Out Unique Coordinate Entries
############################################################

dfUniqueCoordinates <- dfCoordinates %>%
  filter(
    !is.na(estuaryname),
    estuaryname != "",
    !is.na(stationno),
    stationno != "",
    !is.na(latitude),
    !is.na(longitude)
  ) %>%
  distinct(
    estuaryname,
    stationno,
    latitude,
    longitude,
    .keep_all = TRUE
  ) %>%
  group_by(estuaryname, stationno) %>%
  arrange(latitude, longitude, .by_group = TRUE) %>%
  mutate(
    uniqueCoordinateEntry = row_number(),
    numberUniqueCoordinateEntries = n(),
    stationCenterLatitude = mean(latitude, na.rm = TRUE),
    stationCenterLongitude = mean(longitude, na.rm = TRUE)
  ) %>%
  ungroup()

############################################################
### Add Distance From Common Center
############################################################

dfUniqueCoordinates <- dfUniqueCoordinates %>%
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
### Write CSV
############################################################

write.csv(dfUniqueCoordinates, strFullWriteName, row.names = FALSE)

cat("Wrote file to:", strFullWriteName, "\n")
cat("Rows written:", nrow(dfUniqueCoordinates), "\n")

gc()