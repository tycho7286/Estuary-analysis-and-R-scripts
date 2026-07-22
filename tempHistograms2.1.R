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
intImageHeight <- 6
intImageDpi <- 600
strWindowTimeZone <- "UTC"

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
    "26HourNoonCentered_",
    makeSafeFilename(strMetricTitle),
    "_Histogram.png"
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
    "26HourNoonCentered_",
    makeSafeFilename(strMetricTitle),
    "_TimeSeries.png"
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
    expectedFilesInGroupFolder = 10L,
    expectedStackCopies = 1L
  ) %>%
  select(
    groupNumber,
    regionGroup,
    estuaryGroup,
    stationGroup,
    profileGroup,
    seasonGroup,
    expectedFilesInGroupFolder,
    expectedStackCopies,
    outputFolder
  )

write.csv(
  dfOutputPlan,
  file.path(strImagePath, "ExpectedPlotOutputs.csv"),
  row.names = FALSE
)

cat(
  "\nExpected output plan:\n",
  "Each group will contain 4 histograms, 4 time series, 1 summary CSV, and 1 plot stack.\n",
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

  strGroupSubtitle <- paste0(
    "Region = ", strGroupRegion,
    " | Station = ", strGroupStation,
    " | Profile = ", strGroupProfile,
    " | Season = ", strGroupSeason
  )

  strFilePrefix <- paste0(
    makeSafeFilename(strGroupRegion), "_",
    makeSafeFilename(strGroupEstuary), "_Station_",
    makeSafeFilename(strGroupStation), "_Profile_",
    makeSafeFilename(strGroupProfile), "_Season_",
    makeSafeFilename(strGroupSeason), "_"
  )

  strEstuaryFolder <- file.path(
    strImagePath,
    paste0("Region_", makeSafeFilename(strGroupRegion)),
    paste0("Estuary_", makeSafeFilename(strGroupEstuary)),
    paste0("Station_", makeSafeFilename(strGroupStation)),
    paste0("Profile_", makeSafeFilename(strGroupProfile)),
    paste0("Season_", makeSafeFilename(strGroupSeason))
  )

  dir.create(strEstuaryFolder, recursive = TRUE, showWarnings = FALSE)

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

  write.csv(
    dfWindowSummary,
    file.path(
      strEstuaryFolder,
      "26Hour_Noon_Centered_Temperature_Summary.csv"
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
  ### Median and Range Plot Stack
  ############################################################

  plotMedianTimeSeries <- ggplot(
    dfWindowSummary,
    aes(x = windowCenterDateTime, y = medianTemp)
  ) +
    geom_line(linewidth = 0.3, color = "steelblue") +
    labs(
      title = paste0(
        strGroupEstuary,
        ": Median Temperature Time Series"
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
      y = "Median Water Temperature (degrees C)"
    ) +
    theme_minimal()

  plotRangeHistogram <- ggplot(dfWindowSummary, aes(x = tempRange)) +
    geom_histogram(bins = intHistogramBins, color = "white", fill = "steelblue") +
    coord_cartesian(xlim = temperatureRangeLimits) +
    labs(
      title = paste0(strGroupEstuary, ": Temperature Range Histogram"),
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

  plotMedianHistogram <- ggplot(dfWindowSummary, aes(x = medianTemp)) +
    geom_histogram(bins = intHistogramBins, color = "white", fill = "steelblue") +
    coord_cartesian(xlim = temperatureLimits) +
    labs(
      title = paste0(strGroupEstuary, ": Median Temperature Histogram"),
      subtitle = paste0(
        strGroupSubtitle,
        " | ",
        "26-hour windows, n = ",
        format(nrow(dfWindowSummary), big.mark = ",")
      ),
      x = "26-Hour Window Median Water Temperature (degrees C)",
      y = "Number of Windows"
    ) +
    theme_minimal()

  plotMedianRangeStack <- plotMedianTimeSeries /
    plotRangeHistogram /
    plotMedianHistogram

  strStackFilename <- "Median_TimeSeries_Range_Median_Histogram_Stack.png"
  strStackArchiveFilename <- paste0(
    strFilePrefix,
    "Median_TimeSeries_Range_Median_Histogram_Stack.png"
  )
  strStackFullName <- file.path(strEstuaryFolder, strStackFilename)

  ggsave(
    filename = strStackFullName,
    plot = plotMedianRangeStack,
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
    plotMedianTimeSeries,
    plotRangeHistogram,
    plotMedianHistogram,
    plotMedianRangeStack
  )
  gc()
}

cat("Finished writing profile histograms to: ", strImagePath, "\n", sep = "")

gc()
