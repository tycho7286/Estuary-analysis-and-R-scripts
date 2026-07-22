############################################################
### Install and Load Packages
############################################################

# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("patchwork")

library(dplyr)
library(ggplot2)
library(patchwork)

############################################################
### File Paths and Settings
############################################################

# Windows
strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
strImagePath <- "D:/Google/School/2026Summer-BML-UCDGAP/plotsAndImages/rStudioPlotOutputs"

# # Linux
# strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
# strImagePath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/plotsAndImages/rStudioPlotOutputs"

strReadFilename <- "datasetWorkingCopy.rds"

### Use "all" to retain separate groups, "combined" to pool a level, or select one value.
strRegion <- "all"
strEstuary <- "all"
strStation <- "all"
strProfile <- "all"
strSeason <- "all"

intHistogramBins <- 30
intImageWidth <- 8
intImageHeight <- 3
intImageDpi <- 600
strWindowTimeZone <- "UTC"

### Groups must have more than this number of 26-hour windows to be processed.
observationMinCutoff <- 50

strFullName <- file.path(strInPath, strReadFilename)
# estuaryCombined <- readRDS(strFullName)

############################################################
### Helper Functions
############################################################

### Make text safe for use in folder and file names.
makeSafeFilename <- function(strText) {
  strText <- gsub("[^A-Za-z0-9_]+", "_", strText)
  strText <- gsub("_+", "_", strText)
  gsub("^_|_$", "", strText)
}

### Convert text to lower camelCase for output filenames.
makeCamelCase <- function(strText) {
  listWords <- unlist(strsplit(as.character(strText), "[^A-Za-z0-9]+"))
  listWords <- listWords[listWords != ""]

  if (length(listWords) == 0) {
    return("output")
  }

  listWords <- tolower(listWords)
  paste0(
    listWords[1],
    paste0(
      toupper(substr(listWords[-1], 1, 1)),
      substring(listWords[-1], 2),
      collapse = ""
    )
  )
}

### Replace missing or blank grouping values with a readable label.
cleanGroupValue <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- "Not Recorded"
  x
}

### Create and save one 26-hour window summary histogram.
saveSummaryHistogram <- function(
  dfWindowSummary,
  strColumnName,
  strMetricTitle,
  strXAxisLabel,
  strEstuary,
  strSelectedProfile,
  strEstuaryFolder
) {
  dfPlot <- dfWindowSummary %>%
    filter(is.finite(.data[[strColumnName]]))

  if (nrow(dfPlot) == 0) {
    cat("No values available for ", strMetricTitle, ". Skipping plot.\n", sep = "")
    return(invisible(NULL))
  }

  plotHistogram <- ggplot(dfPlot, aes(x = .data[[strColumnName]])) +
    geom_histogram(
      bins = intHistogramBins,
      color = "white",
      fill = "steelblue"
    ) +
    coord_cartesian(
      xlim = if (strColumnName == "tempRange") {
        temperatureRangeLimits
      } else {
        temperatureLimits
      }
    ) +
    labs(
      title = paste0(
        strEstuary,
        ": 26-Hour Window ",
        strMetricTitle,
        " Histogram"
      ),
      subtitle = paste0(
        strGroupSubtitle,
        " | ",
        "Windows centered at 12:00 noon ",
        strWindowTimeZone,
        ", n = ",
        format(nrow(dfPlot), big.mark = ","),
        " windows"
      ),
      x = strXAxisLabel,
      y = "Number of Windows"
    ) +
    theme_minimal()

  strPlotFilename <- paste0(
    makeCamelCase(strMetricTitle),
    "Hist.png"
  )

  ggsave(
    filename = file.path(strEstuaryFolder, strPlotFilename),
    plot = plotHistogram,
    width = intImageWidth,
    height = intImageHeight,
    units = "in",
    dpi = intImageDpi,
    bg = "white"
  )
}

### Create and save one 26-hour window summary frequency polygon.
saveSummaryFrequencyPolygon <- function(
  dfWindowSummary,
  strColumnName,
  strMetricTitle,
  strXAxisLabel,
  strEstuary,
  strSelectedProfile,
  strEstuaryFolder
) {
  dfPlot <- dfWindowSummary %>%
    filter(is.finite(.data[[strColumnName]]))

  if (nrow(dfPlot) == 0) {
    cat("No values available for ", strMetricTitle, ". Skipping frequency polygon.\n", sep = "")
    return(invisible(NULL))
  }

  plotFrequencyPolygon <- ggplot(dfPlot, aes(x = .data[[strColumnName]])) +
    geom_freqpoly(
      bins = intHistogramBins,
      linewidth = 0.8,
      color = "steelblue"
    ) +
    coord_cartesian(
      xlim = if (strColumnName == "tempRange") {
        temperatureRangeLimits
      } else {
        temperatureLimits
      }
    ) +
    labs(
      title = paste0(
        strEstuary,
        ": 26-Hour Window ",
        strMetricTitle,
        " Frequency Polygon"
      ),
      subtitle = paste0(
        strGroupSubtitle,
        " | Windows centered at 12:00 noon ",
        strWindowTimeZone,
        ", n = ",
        format(nrow(dfPlot), big.mark = ","),
        " windows"
      ),
      x = strXAxisLabel,
      y = "Number of Windows"
    ) +
    theme_minimal()

  strPlotFilename <- paste0(
    makeCamelCase(strMetricTitle),
    "FP.png"
  )

  ggsave(
    filename = file.path(strEstuaryFolder, strPlotFilename),
    plot = plotFrequencyPolygon,
    width = intImageWidth,
    height = intImageHeight,
    units = "in",
    dpi = intImageDpi,
    bg = "white"
  )
}

### Overlay the mean histogram and frequency polygon as an axis-alignment check.
saveMeanHistogramFrequencyCheck <- function(
  dfWindowSummary,
  strEstuary,
  strEstuaryFolder
) {
  dfPlot <- dfWindowSummary %>%
    filter(is.finite(meanTemp))

  plotMeanCheck <- ggplot(dfPlot, aes(x = meanTemp)) +
    geom_histogram(
      bins = intHistogramBins,
      color = "white",
      fill = "grey70"
    ) +
    geom_freqpoly(
      bins = intHistogramBins,
      linewidth = 0.8,
      color = "steelblue"
    ) +
    coord_cartesian(xlim = temperatureLimits) +
    labs(
      title = paste0(strEstuary, ": Mean Temperature hist and FP Check"),
      subtitle = paste0(
        strGroupSubtitle,
        " | Same bins and axes, n = ",
        format(nrow(dfPlot), big.mark = ","),
        " windows"
      ),
      x = "26-Hour Window Mean Water Temperature (degrees C)",
      y = "Number of Windows"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(strEstuaryFolder, "meanTemperatureHistFPCheck.png"),
    plot = plotMeanCheck,
    width = intImageWidth,
    height = intImageHeight,
    units = "in",
    dpi = intImageDpi,
    bg = "white"
  )
}

### Create and save one 26-hour window summary time series.
saveSummaryTimeSeries <- function(
  dfWindowSummary,
  strColumnName,
  strMetricTitle,
  strYAxisLabel,
  strEstuary,
  strSelectedProfile,
  strEstuaryFolder
) {
  dfPlot <- dfWindowSummary %>%
    filter(is.finite(.data[[strColumnName]]))

  if (nrow(dfPlot) == 0) {
    cat("No values available for ", strMetricTitle, ". Skipping time series.\n", sep = "")
    return(invisible(NULL))
  }

  plotTimeSeries <- ggplot(
    dfPlot,
    aes(x = windowCenterDateTime, y = .data[[strColumnName]])
  ) +
    geom_line(linewidth = 0.3, color = "steelblue") +
    labs(
      title = paste0(
        strEstuary,
        ": 26-Hour Window ",
        strMetricTitle,
        " Time Series"
      ),
      subtitle = paste0(
        strGroupSubtitle,
        " | ",
        "Windows centered at 12:00 noon ",
        strWindowTimeZone,
        ", n = ",
        format(nrow(dfPlot), big.mark = ",")
      ),
      x = "Window Center Date",
      y = strYAxisLabel
    ) +
    theme_minimal()

  strPlotFilename <- paste0(
    makeCamelCase(strMetricTitle),
    "TimeSeries.png"
  )

  ggsave(
    filename = file.path(strEstuaryFolder, strPlotFilename),
    plot = plotTimeSeries,
    width = intImageWidth,
    height = intImageHeight,
    units = "in",
    dpi = intImageDpi,
    bg = "white"
  )
}

############################################################
### Validate Data
############################################################

requiredFields <- c(
  "region",
  "estuaryname",
  "stationno",
  "profile",
  "season",
  "DateTime",
  "raw_h2otemp"
)
missingFields <- setdiff(requiredFields, names(estuaryCombined))

if (length(missingFields) > 0) {
  stop("Input dataset is missing required fields: ", paste(missingFields, collapse = ", "))
}

strRegion <- trimws(tolower(strRegion))
strEstuary <- trimws(tolower(strEstuary))
strStation <- trimws(tolower(strStation))
strProfile <- trimws(tolower(strProfile))
strSeason <- trimws(tolower(strSeason))

availableProfiles <- estuaryCombined %>%
  filter(!is.na(profile)) %>%
  distinct(profile = trimws(tolower(as.character(profile)))) %>%
  arrange(profile) %>%
  pull(profile)

if (!strProfile %in% c("all", "combined") && !strProfile %in% availableProfiles) {
  stop(
    "Selected profile was not found: ",
    strProfile,
    ". Available profiles: ",
    paste(c("all", "combined", availableProfiles), collapse = ", ")
  )
}

dir.create(strImagePath, recursive = TRUE, showWarnings = FALSE)

strPlotStackPath <- file.path(strImagePath, "zPlotStacks")
dir.create(strPlotStackPath, recursive = TRUE, showWarnings = FALSE)

############################################################
### Prepare Selected Grouping Data
############################################################

dfSelectedProfile <- estuaryCombined %>%
  select(
    region,
    estuaryname,
    stationno,
    profile,
    season,
    DateTime,
    raw_h2otemp
  ) %>%
  mutate(
    regionGroup = cleanGroupValue(region),
    estuaryGroup = cleanGroupValue(estuaryname),
    stationGroup = cleanGroupValue(stationno),
    profileGroup = cleanGroupValue(profile),
    seasonGroup = cleanGroupValue(season),
    raw_h2otemp = suppressWarnings(as.numeric(raw_h2otemp))
  ) %>%
  filter(
    strRegion %in% c("all", "combined") | tolower(regionGroup) == strRegion,
    strEstuary %in% c("all", "combined") | tolower(estuaryGroup) == strEstuary,
    strStation %in% c("all", "combined") | tolower(stationGroup) == strStation,
    strProfile %in% c("all", "combined") | tolower(profileGroup) == strProfile,
    strSeason %in% c("all", "combined") | tolower(seasonGroup) == strSeason,
    !is.na(DateTime),
    is.finite(raw_h2otemp)
  ) %>%
  mutate(
    regionGroup = if (strRegion == "combined") "Combined" else regionGroup,
    estuaryGroup = if (strEstuary == "combined") "Combined" else estuaryGroup,
    stationGroup = if (strStation == "combined") "Combined" else stationGroup,
    profileGroup = if (strProfile == "combined") "Combined" else profileGroup,
    seasonGroup = if (strSeason == "combined") "Combined" else seasonGroup
  ) %>%
  select(
    regionGroup,
    estuaryGroup,
    stationGroup,
    profileGroup,
    seasonGroup,
    DateTime,
    raw_h2otemp
  )

############################################################
### Relabel Single-Depth Estuaries
############################################################

dfSingleDepthEstuaries <- data.frame()

if (strProfile == "all") {
  dfSingleDepthEstuaries <- dfSelectedProfile %>%
    distinct(regionGroup, estuaryGroup, profileGroup) %>%
    group_by(regionGroup, estuaryGroup) %>%
    summarise(
      hasBottom = any(tolower(profileGroup) == "bottom"),
      hasSurface = any(tolower(profileGroup) == "surface"),
      .groups = "drop"
    ) %>%
    filter(hasBottom, !hasSurface)

  if (nrow(dfSingleDepthEstuaries) > 0) {
    for (singleDepthIndex in seq_len(nrow(dfSingleDepthEstuaries))) {
      idxSingleDepth <-
        dfSelectedProfile$regionGroup ==
          dfSingleDepthEstuaries$regionGroup[singleDepthIndex] &
        dfSelectedProfile$estuaryGroup ==
          dfSingleDepthEstuaries$estuaryGroup[singleDepthIndex] &
        tolower(dfSelectedProfile$profileGroup) == "bottom"

      dfSelectedProfile$profileGroup[idxSingleDepth] <- "single depth"
    }

    cat(
      "\nRelabeled bottom as single depth for ",
      nrow(dfSingleDepthEstuaries),
      " estuaries with no surface profile:\n",
      sep = ""
    )
    print(
      as.data.frame(
        dfSingleDepthEstuaries %>%
          select(regionGroup, estuaryGroup)
      ),
      row.names = FALSE
    )
  } else {
    cat("\nNo bottom-only estuaries required single-depth relabeling.\n")
  }
}

if (nrow(dfSelectedProfile) == 0) {
  stop("No usable temperature observations matched the selected grouping filters.")
}

temperatureLimits <- range(dfSelectedProfile$raw_h2otemp, na.rm = TRUE)
temperatureRangeLimits <- c(0, diff(temperatureLimits))

gc()

cat(
  "Standard temperature limits: ",
  temperatureLimits[1],
  " to ",
  temperatureLimits[2],
  " degrees C\n",
  "Standard temperature range limits: ",
  temperatureRangeLimits[1],
  " to ",
  temperatureRangeLimits[2],
  " degrees C\n",
  sep = ""
)

dfPlotGroups <- dfSelectedProfile %>%
  distinct(regionGroup, estuaryGroup, stationGroup, profileGroup, seasonGroup) %>%
  arrange(regionGroup, estuaryGroup, stationGroup, profileGroup, seasonGroup)

dfInputCoverage <- data.frame(
  loadedRows = nrow(estuaryCombined),
  selectedTemperatureRows = nrow(dfSelectedProfile),
  regions = n_distinct(dfSelectedProfile$regionGroup),
  estuaries = n_distinct(dfSelectedProfile$estuaryGroup),
  stations = n_distinct(dfSelectedProfile$stationGroup),
  profiles = n_distinct(dfSelectedProfile$profileGroup),
  seasons = n_distinct(dfSelectedProfile$seasonGroup),
  outputGroups = nrow(dfPlotGroups)
)

cat("\nLoaded-data and output-group coverage:\n")
print(dfInputCoverage, row.names = FALSE)

if (
  all(c(strRegion, strEstuary, strStation, strProfile, strSeason) == "all") &&
    nrow(dfPlotGroups) == 1
) {
  warning(
    paste0(
      "All filters are set to 'all', but only one output group was found. ",
      "The loaded estuaryCombined object may contain only a subset, or the ",
      "data-preparation section was not rerun. Reload the full dataset and ",
      "source the complete script."
    ),
    call. = FALSE
  )
}

dfOutputPlan <- dfPlotGroups %>%
  mutate(
    groupNumber = row_number(),
    outputFolder = file.path(
      strImagePath,
      paste0("Region_", makeSafeFilename(regionGroup)),
      paste0("Estuary_", makeSafeFilename(estuaryGroup)),
      paste0("Station_", makeSafeFilename(stationGroup)),
      paste0("Profile_", makeSafeFilename(profileGroup)),
      paste0("Season_", makeSafeFilename(seasonGroup))
    ),
    expectedFilesInGroupFolder = 15L,
    expectedStackCopies = 1L,
    observationMinCutoff = observationMinCutoff
  ) %>%
  select(
    groupNumber,
    regionGroup,
    estuaryGroup,
    stationGroup,
    profileGroup,
    seasonGroup,
    observationMinCutoff,
    expectedFilesInGroupFolder,
    expectedStackCopies,
    outputFolder
  )

write.csv(
  dfOutputPlan,
  file.path(strImagePath, "expectedPlotOutputs.csv"),
  row.names = FALSE
)

cat(
  "\nExpected output plan:\n",
  "Candidate groups with n <= ",
  observationMinCutoff,
  " 26-hour windows will be skipped.\n",
  "Each group will contain 4 histograms, 4 frequency polygons, 4 time series,\n",
  "1 mean hist and FP check, 1 summary CSV, and 1 plot stack.\n",
  "Each plot stack will also be copied to zPlotStacks.\n",
  "Output groups:\n",
  sep = ""
)
print(as.data.frame(dfOutputPlan), row.names = FALSE)

cat(
  "Creating 26-hour noon-centered plots for ",
  nrow(dfPlotGroups),
  " region, estuary, station, profile, and season groups.\n",
  sep = ""
)

############################################################
### Summarize and Plot Each Group
############################################################

for (i in seq_len(nrow(dfPlotGroups))) {
  strGroupRegion <- dfPlotGroups$regionGroup[i]
  strGroupEstuary <- dfPlotGroups$estuaryGroup[i]
  strGroupStation <- dfPlotGroups$stationGroup[i]
  strGroupProfile <- dfPlotGroups$profileGroup[i]
  strGroupSeason <- dfPlotGroups$seasonGroup[i]
  strGroupProfileDisplay <- tools::toTitleCase(tolower(strGroupProfile))

  strGroupSubtitle <- paste0(
    "Region = ", strGroupRegion,
    " | Station = ", strGroupStation,
    " | ", strGroupProfileDisplay,
    " | ", strGroupSeason
  )

  strFilePrefix <- makeCamelCase(
    paste(
      strGroupRegion,
      strGroupEstuary,
      "station",
      strGroupStation,
      strGroupProfile,
      strGroupSeason
    )
  )

  strEstuaryFolder <- file.path(
    strImagePath,
    paste0("Region_", makeSafeFilename(strGroupRegion)),
    paste0("Estuary_", makeSafeFilename(strGroupEstuary)),
    paste0("Station_", makeSafeFilename(strGroupStation)),
    paste0("Profile_", makeSafeFilename(strGroupProfile)),
    paste0("Season_", makeSafeFilename(strGroupSeason))
  )

  cat(
    "Processing group ",
    i,
    " of ",
    nrow(dfPlotGroups),
    ": ",
    strGroupRegion, " > ",
    strGroupEstuary, " > ",
    strGroupStation, " > ",
    strGroupProfile, " > ",
    strGroupSeason,
    "\n",
    sep = ""
  )

  dfEstuary <- dfSelectedProfile %>%
    filter(
      regionGroup == strGroupRegion,
      estuaryGroup == strGroupEstuary,
      stationGroup == strGroupStation,
      profileGroup == strGroupProfile,
      seasonGroup == strGroupSeason
    ) %>%
    transmute(
      DateTime,
      raw_h2otemp,
      observationDate = as.Date(DateTime, tz = strWindowTimeZone),
      baseNoon = as.POSIXct(
        paste(observationDate, "12:00:00"),
        tz = strWindowTimeZone
      )
    )

  ### Assign every observation to its same-day noon window.
  dfBaseWindow <- dfEstuary %>%
    transmute(
      windowCenterDateTime = baseNoon,
      raw_h2otemp
    )

  ### Add only observations in the overlapping midnight edge hours.
  dfPreviousWindowOverlap <- dfEstuary %>%
    mutate(windowCenterDateTime = baseNoon - 24 * 60 * 60) %>%
    filter(
      DateTime >= windowCenterDateTime - 13 * 60 * 60,
      DateTime <= windowCenterDateTime + 13 * 60 * 60
    ) %>%
    select(windowCenterDateTime, raw_h2otemp)

  dfNextWindowOverlap <- dfEstuary %>%
    mutate(windowCenterDateTime = baseNoon + 24 * 60 * 60) %>%
    filter(
      DateTime >= windowCenterDateTime - 13 * 60 * 60,
      DateTime <= windowCenterDateTime + 13 * 60 * 60
    ) %>%
    select(windowCenterDateTime, raw_h2otemp)

  dfWindowObservations <- bind_rows(
    dfBaseWindow,
    dfPreviousWindowOverlap,
    dfNextWindowOverlap
  )

  dfWindowSummary <- dfWindowObservations %>%
    group_by(windowCenterDateTime) %>%
    summarise(
      observationCount = n(),
      minTemp = min(raw_h2otemp, na.rm = TRUE),
      meanTemp = mean(raw_h2otemp, na.rm = TRUE),
      medianTemp = median(raw_h2otemp, na.rm = TRUE),
      maxTemp = max(raw_h2otemp, na.rm = TRUE),
      tempRange = maxTemp - minTemp,
      .groups = "drop"
    )

  if (nrow(dfWindowSummary) <= observationMinCutoff) {
    cat(
      "Skipping group ",
      i,
      ": n = ",
      nrow(dfWindowSummary),
      " windows is at or below observationMinCutoff = ",
      observationMinCutoff,
      ".\n",
      sep = ""
    )

    rm(
      dfEstuary,
      dfBaseWindow,
      dfPreviousWindowOverlap,
      dfNextWindowOverlap,
      dfWindowObservations,
      dfWindowSummary
    )
    gc()
    next
  }

  dir.create(strEstuaryFolder, recursive = TRUE, showWarnings = FALSE)

  write.csv(
    dfWindowSummary,
    file.path(
      strEstuaryFolder,
      "temperatureSummary.csv"
    ),
    row.names = FALSE
  )

  saveSummaryHistogram(
    dfWindowSummary,
    "maxTemp",
    "Maximum Temperature",
    "26-Hour Window Maximum Water Temperature (degrees C)",
    strGroupEstuary,
    strGroupProfile,
    strEstuaryFolder
  )

  saveSummaryHistogram(
    dfWindowSummary,
    "meanTemp",
    "Mean Temperature",
    "26-Hour Window Mean Water Temperature (degrees C)",
    strGroupEstuary,
    strGroupProfile,
    strEstuaryFolder
  )

  saveSummaryHistogram(
    dfWindowSummary,
    "minTemp",
    "Minimum Temperature",
    "26-Hour Window Minimum Water Temperature (degrees C)",
    strGroupEstuary,
    strGroupProfile,
    strEstuaryFolder
  )

  saveSummaryHistogram(
    dfWindowSummary,
    "tempRange",
    "Temperature Range",
    "26-Hour Window Temperature Range (degrees C)",
    strGroupEstuary,
    strGroupProfile,
    strEstuaryFolder
  )

  saveSummaryFrequencyPolygon(
    dfWindowSummary,
    "maxTemp",
    "Maximum Temperature",
    "26-Hour Window Maximum Water Temperature (degrees C)",
    strGroupEstuary,
    strGroupProfile,
    strEstuaryFolder
  )

  saveSummaryFrequencyPolygon(
    dfWindowSummary,
    "meanTemp",
    "Mean Temperature",
    "26-Hour Window Mean Water Temperature (degrees C)",
    strGroupEstuary,
    strGroupProfile,
    strEstuaryFolder
  )

  saveSummaryFrequencyPolygon(
    dfWindowSummary,
    "minTemp",
    "Minimum Temperature",
    "26-Hour Window Minimum Water Temperature (degrees C)",
    strGroupEstuary,
    strGroupProfile,
    strEstuaryFolder
  )

  saveSummaryFrequencyPolygon(
    dfWindowSummary,
    "tempRange",
    "Temperature Range",
    "26-Hour Window Temperature Range (degrees C)",
    strGroupEstuary,
    strGroupProfile,
    strEstuaryFolder
  )

  saveMeanHistogramFrequencyCheck(
    dfWindowSummary,
    strGroupEstuary,
    strEstuaryFolder
  )

  saveSummaryTimeSeries(
    dfWindowSummary,
    "maxTemp",
    "Maximum Temperature",
    "Maximum Water Temperature (degrees C)",
    strGroupEstuary,
    strGroupProfile,
    strEstuaryFolder
  )

  saveSummaryTimeSeries(
    dfWindowSummary,
    "meanTemp",
    "Mean Temperature",
    "Mean Water Temperature (degrees C)",
    strGroupEstuary,
    strGroupProfile,
    strEstuaryFolder
  )

  saveSummaryTimeSeries(
    dfWindowSummary,
    "minTemp",
    "Minimum Temperature",
    "Minimum Water Temperature (degrees C)",
    strGroupEstuary,
    strGroupProfile,
    strEstuaryFolder
  )

  saveSummaryTimeSeries(
    dfWindowSummary,
    "tempRange",
    "Temperature Range",
    "Temperature Range (degrees C)",
    strGroupEstuary,
    strGroupProfile,
    strEstuaryFolder
  )

  ############################################################
  ### Mean and Range Plot Stack
  ############################################################

  plotMeanTimeSeries <- ggplot(
    dfWindowSummary,
    aes(x = windowCenterDateTime, y = meanTemp)
  ) +
    geom_line(linewidth = 0.3, color = "steelblue") +
    labs(
      title = paste0(
        strGroupEstuary,
        ": Mean Temperature Time Series"
      ),
      subtitle = paste0(
        strGroupSubtitle,
        " | ",
        "26-hour windows centered at 12:00 noon ",
        strWindowTimeZone,
        ", n = ",
        format(nrow(dfWindowSummary), big.mark = ",")
      ),
      x = "Window Center Date",
      y = "Mean Water Temperature (degrees C)"
    ) +
    theme_minimal()

  plotRangeFrequencyPolygon <- ggplot(dfWindowSummary, aes(x = tempRange)) +
    geom_freqpoly(bins = intHistogramBins, linewidth = 0.8, color = "steelblue") +
    coord_cartesian(xlim = temperatureRangeLimits) +
    labs(
      title = paste0(strGroupEstuary, ": Temperature Range Frequency Polygon"),
      subtitle = paste0(
        strGroupSubtitle,
        " | ",
        "26-hour windows, n = ",
        format(nrow(dfWindowSummary), big.mark = ",")
      ),
      x = "26-Hour Window Temperature Range (degrees C)",
      y = "Number of Windows"
    ) +
    theme_minimal()

  plotMeanFrequencyPolygon <- ggplot(dfWindowSummary, aes(x = meanTemp)) +
    geom_freqpoly(bins = intHistogramBins, linewidth = 0.8, color = "steelblue") +
    coord_cartesian(xlim = temperatureLimits) +
    labs(
      title = paste0(strGroupEstuary, ": Mean Temperature Frequency Polygon"),
      subtitle = paste0(
        strGroupSubtitle,
        " | ",
        "26-hour windows, n = ",
        format(nrow(dfWindowSummary), big.mark = ",")
      ),
      x = "26-Hour Window Mean Water Temperature (degrees C)",
      y = "Number of Windows"
    ) +
    theme_minimal()

  plotTimeSeriesRangeMeanStack <- plotMeanTimeSeries /
    plotRangeFrequencyPolygon /
    plotMeanFrequencyPolygon

  strStackFilename <- "meanTimeSeriesRangeFPMeanFPStack.png"
  strStackArchiveFilename <- paste0(
    strFilePrefix,
    "MeanTimeSeriesRangeFPMeanFPStack.png"
  )
  strStackFullName <- file.path(strEstuaryFolder, strStackFilename)

  ggsave(
    filename = strStackFullName,
    plot = plotTimeSeriesRangeMeanStack,
    width = intImageWidth,
    height = intImageHeight * 3,
    units = "in",
    dpi = intImageDpi,
    bg = "white"
  )

  file.copy(
    from = strStackFullName,
    to = file.path(strPlotStackPath, strStackArchiveFilename),
    overwrite = TRUE
  )

  rm(
    dfEstuary,
    dfBaseWindow,
    dfPreviousWindowOverlap,
    dfNextWindowOverlap,
    dfWindowObservations,
    dfWindowSummary,
    plotMeanTimeSeries,
    plotRangeFrequencyPolygon,
    plotMeanFrequencyPolygon,
    plotTimeSeriesRangeMeanStack
  )
  gc()
}

cat("Finished writing profile histograms to: ", strImagePath, "\n", sep = "")

gc()
