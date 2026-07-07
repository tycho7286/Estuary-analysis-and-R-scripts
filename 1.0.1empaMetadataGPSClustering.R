############################################################
### Install and Load Packages
############################################################

# install.packages("dplyr")
# install.packages("dbscan")

library(dplyr)
library(dbscan)

############################################################
### File Paths and Settings
############################################################

strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/metadata"
strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/metadata"

strReadFilename <- "empaStationUniqueCoordinateEntries.csv"

strCompleteWriteFilename <- "empaStationUniqueCoordinateDBSCANComplete.csv"
strReviewWriteFilename <- "empaStationUniqueCoordinateDBSCANReviewOnly.csv"

strFullReadName <- file.path(strInPath, strReadFilename)
strFullCompleteWriteName <- file.path(strOutPath, strCompleteWriteFilename)
strFullReviewWriteName <- file.path(strOutPath, strReviewWriteFilename)

intDBSCANDistanceMeters <- 25
intDBSCANMinPoints <- 2

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

### Calculate maximum distance between any two points.
calculateClusterDiameterMeters <- function(latitude, longitude) {
  if (length(latitude) <= 1) {
    return(0)
  }
  
  maxDistance <- 0
  
  for (i in seq_along(latitude)) {
    for (j in seq_along(latitude)) {
      distance <- calculateDistanceMeters(
        latitude[i],
        longitude[i],
        latitude[j],
        longitude[j]
      )
      
      if (!is.na(distance) && distance > maxDistance) {
        maxDistance <- distance
      }
    }
  }
  
  maxDistance
}

### Run DBSCAN within one estuary station pair.
runDBSCANForStation <- function(dfStation) {
  if (nrow(dfStation) < intDBSCANMinPoints) {
    dfStation$dbscanCluster <- 0
    return(dfStation)
  }
  
  meanLatitude <- mean(dfStation$latitude, na.rm = TRUE)
  
  dfMeters <- dfStation %>%
    mutate(
      xMeters = longitude * 111320 * cos(meanLatitude * pi / 180),
      yMeters = latitude * 110540
    )
  
  dbscanResult <- dbscan(
    dfMeters[, c("xMeters", "yMeters")],
    eps = intDBSCANDistanceMeters,
    minPts = intDBSCANMinPoints
  )
  
  dfStation$dbscanCluster <- dbscanResult$cluster
  
  dfStation
}

############################################################
### Load Unique Coordinate Entries
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
### Run DBSCAN by Estuary Station Pair
############################################################

dfClustered <- dfCoordinates %>%
  group_by(estuaryname, stationno) %>%
  group_modify(~ runDBSCANForStation(.x)) %>%
  ungroup()

############################################################
### Summarize DBSCAN Clusters
############################################################

dfClusterSummary <- dfClustered %>%
  filter(dbscanCluster > 0) %>%
  group_by(estuaryname, stationno, dbscanCluster) %>%
  summarise(
    dbscanClusterCenterLatitude = mean(latitude, na.rm = TRUE),
    dbscanClusterCenterLongitude = mean(longitude, na.rm = TRUE),
    dbscanClusterPointCount = n(),
    dbscanClusterDiameterMeters = calculateClusterDiameterMeters(
      latitude,
      longitude
    ),
    .groups = "drop"
  )

dfOutput <- dfClustered %>%
  left_join(
    dfClusterSummary,
    by = c("estuaryname", "stationno", "dbscanCluster")
  ) %>%
  mutate(
    distanceFromDBSCANClusterCenterMeters = ifelse(
      dbscanCluster > 0,
      calculateDistanceMeters(
        latitude,
        longitude,
        dbscanClusterCenterLatitude,
        dbscanClusterCenterLongitude
      ),
      NA_real_
    )
  ) %>%
  group_by(estuaryname, stationno, dbscanCluster) %>%
  mutate(
    dbscanClusterRadiusMeters = ifelse(
      dbscanCluster > 0,
      max(distanceFromDBSCANClusterCenterMeters, na.rm = TRUE),
      NA_real_
    )
  ) %>%
  ungroup()

############################################################
### Add Station Level Summary Fields
############################################################

dfOutput <- dfOutput %>%
  group_by(estuaryname, stationno) %>%
  mutate(
    numberUniqueCoordinateEntries = n(),
    numberDBSCANClusters = n_distinct(dbscanCluster[dbscanCluster > 0]),
    numberNoisePoints = sum(dbscanCluster == 0),
    stationPointClusterDiameterMeters = calculateClusterDiameterMeters(
      latitude,
      longitude
    )
  ) %>%
  ungroup() %>%
  arrange(
    desc(stationPointClusterDiameterMeters),
    estuaryname,
    suppressWarnings(as.numeric(stationno)),
    stationno,
    dbscanCluster,
    uniqueCoordinateEntry
  )

############################################################
### Create Review Only File
############################################################

dfReview <- dfOutput %>%
  filter(
    numberUniqueCoordinateEntries > 1,
    numberDBSCANClusters > 1 | numberNoisePoints > 0
  )

############################################################
### Write CSV Files
############################################################

write.csv(dfOutput, strFullCompleteWriteName, row.names = FALSE)
write.csv(dfReview, strFullReviewWriteName, row.names = FALSE)

cat("Wrote complete DBSCAN file to:", strFullCompleteWriteName, "\n")
cat("Rows written:", nrow(dfOutput), "\n\n")

cat("Wrote review only DBSCAN file to:", strFullReviewWriteName, "\n")
cat("Rows written:", nrow(dfReview), "\n\n")

cat("DBSCAN distance in meters:", intDBSCANDistanceMeters, "\n")
cat("DBSCAN minimum points:", intDBSCANMinPoints, "\n")

gc()