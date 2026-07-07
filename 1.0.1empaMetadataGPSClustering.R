############################################################
### Install and Load Packages
############################################################

# install.packages("dplyr")
# install.packages("dbscan")

library(dplyr)
library(dbscan)

############################################################
### File Paths
############################################################

strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/metadata"
strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/metadata"

strReadFilename <- "empaStationUniqueCoordinateEntries.csv"
strWriteFilename <- "empaStationCoordinateClusters.csv"

strFullReadName <- file.path(strInPath, strReadFilename)
strFullWriteName <- file.path(strOutPath, strWriteFilename)

############################################################
### DBSCAN Settings
############################################################

### Maximum distance between neighboring points in meters.
intDBSCANDistanceMeters <- 25

### Minimum number of points required to form a cluster.
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

### Calculate maximum distance between any two coordinate points.
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

### Convert latitude and longitude to approximate local meter coordinates.
createInPoints <- function(dfStation) {
  meanLatitude <- mean(dfStation$latitude, na.rm = TRUE)
  
  inPoints <- data.frame(
    x = dfStation$longitude * 111320 * cos(meanLatitude * pi / 180),
    y = dfStation$latitude * 110540
  )
  
  inPoints
}

### Run DBSCAN within one estuary station pair.
runDBSCANForStation <- function(dfStation) {
  dfStation <- dfStation %>%
    arrange(latitude, longitude)
  
  if (nrow(dfStation) < intDBSCANMinPoints) {
    dfStation$dbscanCluster <- 0
    return(dfStation)
  }
  
  inPoints <- createInPoints(dfStation)
  
  dbscanResult <- dbscan(
    inPoints,
    eps = intDBSCANDistanceMeters,
    minPts = intDBSCANMinPoints
  )
  
  dfStation$dbscanCluster <- dbscanResult$cluster
  
  dfStation
}

############################################################
### Load Coordinate Entries
############################################################

dfCoordinates <- read.csv(strFullReadName, stringsAsFactors = FALSE)

dfCoordinates <- dfCoordinates %>%
  mutate(
    estuaryname = trimws(as.character(estuaryname)),
    stationno = trimws(as.character(stationno)),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude))
  )

if ("profile" %in% names(dfCoordinates)) {
  dfCoordinates$profile <- trimws(as.character(dfCoordinates$profile))
}

############################################################
### Check Required Fields
############################################################

requiredFields <- c(
  "estuaryname",
  "stationno",
  "latitude",
  "longitude",
  "uniqueCoordinateEntry"
)

missingFields <- setdiff(requiredFields, names(dfCoordinates))

if (length(missingFields) > 0) {
  stop(
    "Missing required fields in coordinate entry file: ",
    paste(missingFields, collapse = ", ")
  )
}

############################################################
### Filter Valid Coordinate Entries
############################################################

dfCoordinates <- dfCoordinates %>%
  filter(
    !is.na(estuaryname),
    estuaryname != "",
    !is.na(stationno),
    stationno != "",
    !is.na(latitude),
    !is.na(longitude)
  )

############################################################
### Run DBSCAN by Estuary Station Pair
############################################################

dfClustered <- dfCoordinates %>%
  group_by(estuaryname, stationno) %>%
  group_modify(~ runDBSCANForStation(.x)) %>%
  ungroup()

############################################################
### Summarize Non-Noise DBSCAN Clusters
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

############################################################
### Add Cluster Summary Fields
############################################################

dfOutput <- dfClustered %>%
  left_join(
    dfClusterSummary,
    by = c("estuaryname", "stationno", "dbscanCluster")
  ) %>%
  mutate(
    dbscanIsNoise = dbscanCluster == 0,
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
    numberCoordinateClusters = n_distinct(dbscanCluster[dbscanCluster > 0]),
    numberNoisePoints = sum(dbscanCluster == 0),
    stationCoordinateDiameterMeters = calculateClusterDiameterMeters(
      latitude,
      longitude
    )
  ) %>%
  ungroup()

############################################################
### Arrange Output
############################################################

dfOutput <- dfOutput %>%
  arrange(
    desc(stationCoordinateDiameterMeters),
    estuaryname,
    suppressWarnings(as.numeric(stationno)),
    stationno,
    dbscanCluster,
    uniqueCoordinateEntry
  )

############################################################
### Write CSV
############################################################

write.csv(dfOutput, strFullWriteName, row.names = FALSE)

cat("Wrote coordinate cluster file to:", strFullWriteName, "\n")
cat("Rows written:", nrow(dfOutput), "\n")
cat("DBSCAN distance in meters:", intDBSCANDistanceMeters, "\n")
cat("DBSCAN minimum points:", intDBSCANMinPoints, "\n")

gc()
