## Import and Load Packages

# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("geosphere")
# install.packages("tidyr")

library(dplyr)
library(ggplot2)
library(httr)
library(jsonlite)
library(geosphere)
library(tidyr)

# Windows
strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
strImagePath <- "D:/Google/School/2026Summer-BML-UCDGAP/plotsAndImages/depthSwing"

# # Linux
# strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
# strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
# strImagePath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/plotsAndImages/depthSwing"

strReadFilename <- "datasetWorkingCopy.rds"
strDepthSwingFilename <- "dailyDepthSwing26Hour_StateParks.rds"
strDepthSwingCsvFilename <- "dailyDepthSwing26Hour_StateParks.csv"
strNoaaStationLookupFilename <- "nearestNoaaStations_StateParks.csv"
strNoaaRawFilename <- "noaaHistoricalWaterLevelRaw_StateParks.rds"
strNoaaSwingFilename <- "noaaHistoricalWaterLevelSwing26Hour_StateParks.rds"
strNoaaSwingCsvFilename <- "noaaHistoricalWaterLevelSwing26Hour_StateParks.csv"

strFullName <- file.path(strInPath, strReadFilename)

estuaryCombined <- readRDS(strFullName)

dir.create(strOutPath, recursive = TRUE, showWarnings = FALSE)
dir.create(strImagePath, recursive = TRUE, showWarnings = FALSE)

strProject <- "State-Parks"
intHoursBeforeNoon <- 13
intHoursAfterNoon <- 13
intMinRecordsPerWindow <- 3
intMaxNoaaDaysPerRequest <- 30
intMaxCandidateNoaaStations <- 100
strNoaaDatum <- "MLLW"

############################################################
### Helper Functions
############################################################

### Make safe file names.
makeSafeFilename <- function(strText) {
  strText <- gsub("[^A-Za-z0-9_]+", "_", strText)
  strText <- gsub("_+", "_", strText)
  strText <- gsub("^_|_$", "", strText)
  strText
}

### Format NOAA dates.
formatNoaaDate <- function(dateValue) {
  format(as.Date(dateValue), "%Y%m%d")
}

### Filter dataset for one project.
filterProject <- function(df, strProject) {
  df %>%
    filter(
      projectid == strProject,
      !is.na(estuaryname),
      !is.na(DateTime),
      !is.na(calculatedWaterDepthMeters),
      !is.na(latitude),
      !is.na(longitude)
    )
}

### Make estuary order from latitude.
makeEstuaryOrder <- function(df) {
  dfOrder <- df %>%
    filter(!is.na(latitude)) %>%
    group_by(estuaryname) %>%
    summarise(latitude = mean(latitude, na.rm = TRUE), .groups = "drop") %>%
    arrange(latitude)

  dfOrder$estuaryname
}

### Split a date range into chunks for NOAA requests.
makeDateChunks <- function(dateStart, dateEnd, intMaxDays) {
  startDates <- seq(as.Date(dateStart), as.Date(dateEnd), by = paste(intMaxDays, "days"))
  endDates <- pmin(startDates + intMaxDays - 1, as.Date(dateEnd))

  data.frame(
    dateStart = startDates,
    dateEnd = endDates
  )
}

### Fetch NOAA station metadata.
fetchNoaaStations <- function() {
  strUrl <- "https://api.tidesandcurrents.noaa.gov/mdapi/prod/webapi/stations.json"

  response <- GET(strUrl)

  if (status_code(response) != 200) {
    stop("NOAA station metadata request failed.")
  }

  contentText <- content(response, as = "text", encoding = "UTF-8")
  contentJson <- fromJSON(contentText)

  dfStations <- contentJson$stations

  dfStations <- dfStations %>%
    filter(!is.na(lat), !is.na(lng)) %>%
    mutate(
      lat = suppressWarnings(as.numeric(lat)),
      lng = suppressWarnings(as.numeric(lng))
    ) %>%
    filter(!is.na(lat), !is.na(lng))

  dfStations
}

### Find nearest NOAA station from estuary coordinates.
findNearestNoaaStation <- function(estuaryLat, estuaryLon, dfNoaaStations) {
  distancesMeters <- geosphere::distHaversine(
    p1 = c(estuaryLon, estuaryLat),
    p2 = dfNoaaStations[, c("lng", "lat")]
  )

  nearestIndex <- which.min(distancesMeters)

  data.frame(
    noaaStationId = as.character(dfNoaaStations$id[nearestIndex]),
    noaaStationName = as.character(dfNoaaStations$name[nearestIndex]),
    noaaLatitude = dfNoaaStations$lat[nearestIndex],
    noaaLongitude = dfNoaaStations$lng[nearestIndex],
    noaaDistanceMeters = distancesMeters[nearestIndex],
    stringsAsFactors = FALSE
  )
}

### Check whether a NOAA station has historical water-level data.
stationHasHistoricalWaterLevel <- function(strStationId, dateStart, dateEnd) {
  dateStart <- as.Date(dateStart)
  dateEnd <- as.Date(dateEnd)
  intDateSpan <- as.numeric(dateEnd - dateStart)
  
  dateSamples <- as.Date(c(
    dateStart,
    dateStart + round(intDateSpan * 0.25),
    dateStart + round(intDateSpan * 0.50),
    dateStart + round(intDateSpan * 0.75),
    dateEnd - 3
  ))
  
  dateSamples <- sort(unique(dateSamples[!is.na(dateSamples)]))
  
  for (dateSample in dateSamples) {
    sampleStart <- max(dateStart, dateSample)
    sampleEnd <- min(dateEnd, dateSample + 3)
    
    dfTest <- fetchNoaaWaterLevel(
      strStationId = strStationId,
      dateStart = sampleStart,
      dateEnd = sampleEnd
    )
    
    if (nrow(dfTest) > 0) {
      return(TRUE)
    }
  }
  
  FALSE
}

### Find nearest NOAA station that has historical water level data.
findNearestNoaaWaterLevelStation <- function(estuaryLat, estuaryLon, dfNoaaStations, dateStart, dateEnd) {
  dfCandidateStations <- dfNoaaStations %>%
    mutate(
      noaaDistanceMeters = geosphere::distHaversine(
        p1 = c(estuaryLon, estuaryLat),
        p2 = cbind(lng, lat)
      )
    ) %>%
    arrange(noaaDistanceMeters)
  
  intCandidatesToTry <- min(intMaxCandidateNoaaStations, nrow(dfCandidateStations))
  
  for (index in seq_len(intCandidatesToTry)) {
    testStationId <- as.character(dfCandidateStations$id[index])
    testStationName <- as.character(dfCandidateStations$name[index])
    
    cat(
      "  Testing NOAA station ",
      index,
      " of ",
      intCandidatesToTry,
      ": ",
      testStationName,
      " ",
      testStationId,
      " (",
      round(dfCandidateStations$noaaDistanceMeters[index] / 1000, 1),
      " km)\n",
      sep = ""
    )
    
    hasData <- stationHasHistoricalWaterLevel(
      strStationId = testStationId,
      dateStart = dateStart,
      dateEnd = dateEnd
    )
    
    if (hasData) {
      return(data.frame(
        noaaStationId = testStationId,
        noaaStationName = testStationName,
        noaaLatitude = dfCandidateStations$lat[index],
        noaaLongitude = dfCandidateStations$lng[index],
        noaaDistanceMeters = dfCandidateStations$noaaDistanceMeters[index],
        noaaStationRankTested = index,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  warning("No nearby NOAA station with historical water level data was found.")
  
  data.frame(
    noaaStationId = NA_character_,
    noaaStationName = NA_character_,
    noaaLatitude = NA_real_,
    noaaLongitude = NA_real_,
    noaaDistanceMeters = NA_real_,
    noaaStationRankTested = NA_integer_,
    stringsAsFactors = FALSE
  )
}

### Fetch historical NOAA water level for one station and one date chunk.
fetchNoaaWaterLevelChunk <- function(strStationId, dateStart, dateEnd) {
  strUrl <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"

  response <- GET(
    url = strUrl,
    query = list(
      begin_date = formatNoaaDate(dateStart),
      end_date = formatNoaaDate(dateEnd),
      station = strStationId,
      product = "water_level",
      datum = strNoaaDatum,
      units = "metric",
      time_zone = "gmt",
      format = "json",
      application = "BML_UCDGAP"
    )
  )

  if (status_code(response) != 200) {
    warning(paste("NOAA water level request failed for station", strStationId))
    return(data.frame())
  }

  contentText <- content(response, as = "text", encoding = "UTF-8")
  contentJson <- fromJSON(contentText)

  if ("error" %in% names(contentJson)) {
    warning(paste("NOAA returned an error for station", strStationId, ":", contentText))
    return(data.frame())
  }

  if (!"data" %in% names(contentJson)) {
    warning(paste("No historical water level data returned for station", strStationId))
    return(data.frame(DateTime = as.POSIXct(character()), waterLevelMeters = numeric()))
  }

  dfTide <- contentJson$data

  if (nrow(dfTide) == 0) {
    warning(paste("Empty NOAA water level data returned for station", strStationId))
    return(data.frame(DateTime = as.POSIXct(character()), waterLevelMeters = numeric()))
  }

  if (!all(c("t", "v") %in% names(dfTide))) {
    warning(paste("NOAA water level data missing t or v for station", strStationId))
    return(data.frame(DateTime = as.POSIXct(character()), waterLevelMeters = numeric()))
  }

  dfTide$DateTime <- as.POSIXct(
    dfTide$t,
    format = "%Y-%m-%d %H:%M",
    tz = "UTC"
  )

  dfTide$waterLevelMeters <- suppressWarnings(as.numeric(dfTide$v))

  if (!"DateTime" %in% names(dfTide) | !"waterLevelMeters" %in% names(dfTide)) {
    warning(paste("NOAA DateTime or waterLevelMeters could not be created for station", strStationId))
    return(data.frame(DateTime = as.POSIXct(character()), waterLevelMeters = numeric()))
  }

  dfTide <- dfTide %>%
    filter(!is.na(DateTime), !is.na(waterLevelMeters)) %>%
    distinct(DateTime, .keep_all = TRUE) %>%
    select(DateTime, waterLevelMeters, everything())

  if (nrow(dfTide) == 0) {
    warning(paste("NOAA water level data had no usable rows for station", strStationId))
    return(data.frame(DateTime = as.POSIXct(character()), waterLevelMeters = numeric()))
  }

  dfTide
}

### Fetch historical NOAA water level over a long date range.
fetchNoaaWaterLevel <- function(strStationId, dateStart, dateEnd) {
  dfDateChunks <- makeDateChunks(dateStart, dateEnd, intMaxNoaaDaysPerRequest)
  waterLevelList <- list()

  for (index in seq_len(nrow(dfDateChunks))) {
    cat(
      "  NOAA chunk ",
      index,
      " of ",
      nrow(dfDateChunks),
      ": ",
      as.character(dfDateChunks$dateStart[index]),
      " to ",
      as.character(dfDateChunks$dateEnd[index]),
      "\n",
      sep = ""
    )

    dfChunk <- fetchNoaaWaterLevelChunk(
      strStationId = strStationId,
      dateStart = dfDateChunks$dateStart[index],
      dateEnd = dfDateChunks$dateEnd[index]
    )

    if (nrow(dfChunk) > 0) {
      waterLevelList[[index]] <- dfChunk
    }
  }

  dfWaterLevel <- bind_rows(waterLevelList)

  if (nrow(dfWaterLevel) == 0) {
    warning(paste("No usable NOAA water level rows found for station", strStationId))
    return(data.frame(DateTime = as.POSIXct(character()), waterLevelMeters = numeric()))
  }

  if (!"DateTime" %in% names(dfWaterLevel)) {
    warning(paste("No DateTime column found after binding NOAA chunks for station", strStationId))
    return(data.frame(DateTime = as.POSIXct(character()), waterLevelMeters = numeric()))
  }

  dfWaterLevel %>%
    filter(!is.na(DateTime), !is.na(waterLevelMeters)) %>%
    distinct(DateTime, .keep_all = TRUE) %>%
    arrange(DateTime)
}

### Calculate daily 26-hour depth swing centered on noon.
makeDailyDepthSwing <- function(df) {
  dateList <- sort(unique(as.Date(df$DateTime, tz = "UTC")))
  dailySwingList <- list()

  for (index in seq_along(dateList)) {
    dateCenter <- dateList[index]

    timeNoon <- as.POSIXct(
      paste(dateCenter, "12:00:00"),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )

    timeWindowStart <- timeNoon - intHoursBeforeNoon * 60 * 60
    timeWindowEnd <- timeNoon + intHoursAfterNoon * 60 * 60

    dfWindow <- df %>%
      filter(DateTime >= timeWindowStart, DateTime <= timeWindowEnd)

    if (nrow(dfWindow) > 0) {
      dailySwingList[[index]] <- dfWindow %>%
        group_by(projectid, estuaryname) %>%
        summarise(
          dateCenter = dateCenter,
          timeNoon = timeNoon,
          timeWindowStart = timeWindowStart,
          timeWindowEnd = timeWindowEnd,
          n = n(),
          minDepthMeters = min(calculatedWaterDepthMeters, na.rm = TRUE),
          maxDepthMeters = max(calculatedWaterDepthMeters, na.rm = TRUE),
          depthSwingMeters = maxDepthMeters - minDepthMeters,
          .groups = "drop"
        )
    }
  }

  bind_rows(dailySwingList)
}

### Calculate daily 26-hour NOAA water level swing centered on noon.
makeDailyNoaaSwing <- function(df) {
  dateList <- sort(unique(as.Date(df$DateTime, tz = "UTC")))
  dailySwingList <- list()

  for (index in seq_along(dateList)) {
    dateCenter <- dateList[index]

    timeNoon <- as.POSIXct(
      paste(dateCenter, "12:00:00"),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )

    timeWindowStart <- timeNoon - intHoursBeforeNoon * 60 * 60
    timeWindowEnd <- timeNoon + intHoursAfterNoon * 60 * 60

    dfWindow <- df %>%
      filter(DateTime >= timeWindowStart, DateTime <= timeWindowEnd)

    if (nrow(dfWindow) > 0) {
      dailySwingList[[index]] <- dfWindow %>%
        summarise(
          dateCenter = dateCenter,
          timeNoon = timeNoon,
          timeWindowStart = timeWindowStart,
          timeWindowEnd = timeWindowEnd,
          n = n(),
          minNoaaWaterLevelMeters = min(waterLevelMeters, na.rm = TRUE),
          maxNoaaWaterLevelMeters = max(waterLevelMeters, na.rm = TRUE),
          noaaSwingMeters = maxNoaaWaterLevelMeters - minNoaaWaterLevelMeters,
          .groups = "drop"
        )
    }
  }

  bind_rows(dailySwingList)
}

### Make combined estuary and NOAA swing plot for one estuary.
makeCombinedSwingPlot <- function(dfPlot, strEstuary, strNoaaStationName, strNoaaStationId) {
  ggplot(dfPlot, aes(x = dateCenter, y = swingMeters, color = dataSource)) +
    geom_line(linewidth = 0.3) +
    geom_point(size = 0.7) +
    scale_color_manual(
      values = c(
        "Estuary depth swing" = "blue",
        "NOAA water level swing" = "red"
      )
    ) +
    labs(
      title = paste0(
        strEstuary,
        " 26-Hour Depth Swing Compared With NOAA Station ",
        strNoaaStationName,
        " (",
        strNoaaStationId,
        ")"
      ),
      subtitle = paste0(
        "Window: ",
        intHoursBeforeNoon,
        " hours before noon to ",
        intHoursAfterNoon,
        " hours after noon. NOAA product: historical water_level, datum: ",
        strNoaaDatum
      ),
      x = "Date",
      y = "Swing (m)",
      color = "Data Source"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

############################################################
### Prepare Estuary Depth Data
############################################################

if (!"calculatedWaterDepthMeters" %in% names(estuaryCombined)) {
  stop("calculatedWaterDepthMeters column not found. Run the calculation script first.")
}

if (!"DateTime" %in% names(estuaryCombined)) {
  stop("DateTime column not found.")
}

if (!"latitude" %in% names(estuaryCombined) | !"longitude" %in% names(estuaryCombined)) {
  stop("latitude and longitude columns not found. Add metadata during cleaning before running this script.")
}

dfDepthFull <- filterProject(estuaryCombined, strProject)

if (nrow(dfDepthFull) == 0) {
  stop("No State Parks records found with DateTime, calculatedWaterDepthMeters, latitude, and longitude.")
}

dfDepthFull$calculatedWaterDepthMeters <- suppressWarnings(as.numeric(dfDepthFull$calculatedWaterDepthMeters))

dfDepthFull <- dfDepthFull %>%
  filter(!is.na(calculatedWaterDepthMeters))

listEstuaryOrder <- makeEstuaryOrder(dfDepthFull)

cat("The following", length(listEstuaryOrder), "State Parks estuaries will be processed:\n")
print(listEstuaryOrder)

############################################################
### Calculate Estuary Daily Depth Swing
############################################################

dfDailyDepthSwing <- makeDailyDepthSwing(dfDepthFull) %>%
  filter(!is.na(depthSwingMeters), n >= intMinRecordsPerWindow) %>%
  mutate(estuarynameOrdered = factor(estuaryname, levels = listEstuaryOrder))

if (nrow(dfDailyDepthSwing) == 0) {
  stop("No daily depth swing values calculated. Check depth data and DateTime values.")
}

saveRDS(dfDailyDepthSwing, file.path(strOutPath, strDepthSwingFilename))
write.csv(dfDailyDepthSwing, file.path(strOutPath, strDepthSwingCsvFilename), row.names = FALSE)

dateStart <- min(dfDailyDepthSwing$dateCenter, na.rm = TRUE) - 1
dateEnd <- max(dfDailyDepthSwing$dateCenter, na.rm = TRUE) + 1

cat("NOAA historical water level date range:\n")
print(dateStart)
print(dateEnd)

############################################################
### Find Nearest NOAA Stations
############################################################

dfNoaaStations <- fetchNoaaStations()

dfEstuaryCoordinates <- dfDepthFull %>%
  group_by(estuaryname) %>%
  summarise(
    latitude = mean(latitude, na.rm = TRUE),
    longitude = mean(longitude, na.rm = TRUE),
    .groups = "drop"
  )

dfNearestNoaaStations <- bind_rows(
  lapply(seq_len(nrow(dfEstuaryCoordinates)), function(index) {
    
    cat(
      "
Finding nearest NOAA historical water level station for ",
      dfEstuaryCoordinates$estuaryname[index],
      "
",
      sep = ""
    )
    
    dfNearest <- findNearestNoaaWaterLevelStation(
      estuaryLat = dfEstuaryCoordinates$latitude[index],
      estuaryLon = dfEstuaryCoordinates$longitude[index],
      dfNoaaStations = dfNoaaStations,
      dateStart = dateStart,
      dateEnd = dateEnd
    )
    
    cbind(dfEstuaryCoordinates[index, ], dfNearest)
  })
)

write.csv(
  dfNearestNoaaStations,
  file.path(strOutPath, strNoaaStationLookupFilename),
  row.names = FALSE
)

print(dfNearestNoaaStations)

############################################################
### Fetch NOAA Historical Water Levels and Calculate Swings
############################################################

noaaRawList <- list()
noaaSwingList <- list()

for (index in seq_len(nrow(dfNearestNoaaStations))) {

  strEstuary <- dfNearestNoaaStations$estuaryname[index]
  strStationId <- dfNearestNoaaStations$noaaStationId[index]
  strStationName <- dfNearestNoaaStations$noaaStationName[index]
  
  if (is.na(strStationId)) {
    warning(paste("Skipping", strEstuary, "because no NOAA historical water level station was found."))
    next
  }

  cat(
    "\nFetching NOAA historical water levels for ",
    strEstuary,
    " using ",
    strStationName,
    " station ",
    strStationId,
    "\n",
    sep = ""
  )

  dfNoaaRaw <- fetchNoaaWaterLevel(
    strStationId = strStationId,
    dateStart = dateStart,
    dateEnd = dateEnd
  )

  if (nrow(dfNoaaRaw) == 0) {
    warning(paste("No NOAA historical water level data found for", strEstuary))
    next
  }

  dfNoaaRaw$estuaryname <- strEstuary
  dfNoaaRaw$noaaStationId <- strStationId
  dfNoaaRaw$noaaStationName <- strStationName

  noaaRawList[[strEstuary]] <- dfNoaaRaw

  dfNoaaSwing <- makeDailyNoaaSwing(dfNoaaRaw) %>%
    mutate(
      estuaryname = strEstuary,
      noaaStationId = strStationId,
      noaaStationName = strStationName
    ) %>%
    filter(!is.na(noaaSwingMeters), n >= intMinRecordsPerWindow)

  noaaSwingList[[strEstuary]] <- dfNoaaSwing
}

dfNoaaRawAll <- bind_rows(noaaRawList)
dfNoaaSwingAll <- bind_rows(noaaSwingList)

if (nrow(dfNoaaSwingAll) == 0) {
  stop("No NOAA historical water level swing values were calculated.")
}

saveRDS(dfNoaaRawAll, file.path(strOutPath, strNoaaRawFilename))
saveRDS(dfNoaaSwingAll, file.path(strOutPath, strNoaaSwingFilename))
write.csv(dfNoaaSwingAll, file.path(strOutPath, strNoaaSwingCsvFilename), row.names = FALSE)

############################################################
### Combine Estuary and NOAA Daily Swing Data
############################################################

dfCombinedSwing <- dfDailyDepthSwing %>%
  select(estuaryname, dateCenter, depthSwingMeters) %>%
  left_join(
    dfNoaaSwingAll %>%
      select(estuaryname, dateCenter, noaaSwingMeters, noaaStationId, noaaStationName),
    by = c("estuaryname", "dateCenter")
  )

dfLongSwing <- dfCombinedSwing %>%
  pivot_longer(
    cols = c(depthSwingMeters, noaaSwingMeters),
    names_to = "swingType",
    values_to = "swingMeters"
  ) %>%
  mutate(
    dataSource = case_when(
      swingType == "depthSwingMeters" ~ "Estuary depth swing",
      swingType == "noaaSwingMeters" ~ "NOAA water level swing",
      TRUE ~ swingType
    )
  ) %>%
  filter(!is.na(swingMeters))

write.csv(
  dfCombinedSwing,
  file.path(strOutPath, "combinedEstuaryNoaaSwing26Hour_StateParks.csv"),
  row.names = FALSE
)

############################################################
### Plot Each Estuary Separately
############################################################

for (strEstuary in unique(dfLongSwing$estuaryname)) {

  dfPlot <- dfLongSwing %>%
    filter(estuaryname == strEstuary)

  if (nrow(dfPlot) == 0) {
    next
  }

  strStationName <- unique(dfPlot$noaaStationName[!is.na(dfPlot$noaaStationName)])[1]
  strStationId <- unique(dfPlot$noaaStationId[!is.na(dfPlot$noaaStationId)])[1]

  if (is.na(strStationName)) {
    strStationName <- "No NOAA station"
  }

  if (is.na(strStationId)) {
    strStationId <- "NA"
  }

  plotCombinedSwing <- makeCombinedSwingPlot(
    dfPlot = dfPlot,
    strEstuary = strEstuary,
    strNoaaStationName = strStationName,
    strNoaaStationId = strStationId
  )

  print(plotCombinedSwing)

  ggsave(
    filename = file.path(
      strImagePath,
      paste0(makeSafeFilename(strEstuary), "_EstuaryDepthSwing_NOAAHistoricalWaterLevelSwing.png")
    ),
    plot = plotCombinedSwing,
    width = 12,
    height = 6,
    units = "in",
    dpi = 600,
    bg = "white"
  )
}

############################################################
### Plot All Estuaries Together
############################################################

dfLongSwing <- dfLongSwing %>%
  mutate(estuarynameOrdered = factor(estuaryname, levels = listEstuaryOrder))

plotAllCombinedSwing <- ggplot(dfLongSwing, aes(x = dateCenter, y = swingMeters, color = dataSource)) +
  geom_line(linewidth = 0.25) +
  geom_point(size = 0.5) +
  scale_color_manual(
    values = c(
      "Estuary depth swing" = "blue",
      "NOAA water level swing" = "red"
    )
  ) +
  facet_wrap(~ estuarynameOrdered, scales = "free_y") +
  labs(
    title = "State Parks Daily 26-Hour Estuary Depth Swing and NOAA Historical Water Level Swing",
    subtitle = paste0(
      "Blue = estuary calculated water depth swing, red = nearest NOAA station historical water_level swing. Datum: ",
      strNoaaDatum
    ),
    x = "Date",
    y = "Swing (m)",
    color = "Data Source"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    axis.text.y = element_text(size = 6)
  )

print(plotAllCombinedSwing)

ggsave(
  filename = file.path(strImagePath, "StateParks_EstuaryDepthSwing_NOAAHistoricalWaterLevelSwing_AllEstuaries.png"),
  plot = plotAllCombinedSwing,
  width = 18,
  height = 24,
  units = "in",
  dpi = 600,
  bg = "white"
)

gc()
