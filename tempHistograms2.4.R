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

### Use c("all"), c("combined"), or list one or more specific values.
strRegion <- c("Baja", "North", "Central", "South")
strEstuary <- c("all")
strStation <- c("all")
strProfile <- c("bottom", "deep", "surface")
strSeason <- c("Spring", "Fall")

### Example: strRegion <- c("Baja", "South", "Central Coast")

### Set to TRUE to relabel both deep and bottom observations as bottom.
bolCombineDeepAndBottom <- TRUE

### Absolute bin widths keep every group's histogram and FP directly comparable.
dblTemperatureBinWidth <- 0.5
dblTemperatureRangeBinWidth <- 0.5
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

### Normalize a selector vector and prevent ambiguous control-value combinations.
normalizeSelector <- function(selector, selectorName) {
  selector <- unique(trimws(tolower(as.character(selector))))
  selector <- selector[!is.na(selector) & selector != ""]

  if (length(selector) == 0) {
    stop(selectorName, " must contain at least one selection.")
  }

  controlValues <- selector %in% c("all", "combined")

  if (sum(controlValues) > 1 || (any(controlValues) && length(selector) > 1)) {
    stop(
      selectorName,
      " must use 'all' or 'combined' alone, or contain only specific values."
    )
  }

  selector
}

### Return TRUE when a selector uses one control value.
selectorIs <- function(selector, controlValue) {
  length(selector) == 1 && identical(selector, controlValue)
}

### Select every row for all/combined, otherwise match any requested value.
selectorMatches <- function(values, selector) {
  if (selectorIs(selector, "all") || selectorIs(selector, "combined")) {
    return(rep(TRUE, length(values)))
  }

  tolower(values) %in% selector
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
      binwidth = if (strColumnName == "tempRange") {
        dblTemperatureRangeBinWidth
      } else {
        dblTemperatureBinWidth
      },
      boundary = 0,
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
      binwidth = if (strColumnName == "tempRange") {
        dblTemperatureRangeBinWidth
      } else {
        dblTemperatureBinWidth
      },
      boundary = 0,
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
      binwidth = dblTemperatureBinWidth,
      boundary = 0,
      color = "white",
      fill = "grey70"
    ) +
    geom_freqpoly(
      binwidth = dblTemperatureBinWidth,
      boundary = 0,
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

strRegion <- normalizeSelector(strRegion, "strRegion")
strEstuary <- normalizeSelector(strEstuary, "strEstuary")
strStation <- normalizeSelector(strStation, "strStation")
strProfile <- normalizeSelector(strProfile, "strProfile")
strSeason <- normalizeSelector(strSeason, "strSeason")

if (length(bolCombineDeepAndBottom) != 1 || is.na(bolCombineDeepAndBottom)) {
  stop("bolCombineDeepAndBottom must be either TRUE or FALSE.")
}

cat(
  "\nSelections:\n",
  "  Regions: ", paste(strRegion, collapse = ", "), "\n",
  "  Estuaries: ", paste(strEstuary, collapse = ", "), "\n",
  "  Stations: ", paste(strStation, collapse = ", "), "\n",
  "  Profiles: ", paste(strProfile, collapse = ", "), "\n",
  "  Seasons: ", paste(strSeason, collapse = ", "), "\n",
  "  Combine deep and bottom: ", bolCombineDeepAndBottom, "\n",
  sep = ""
)

availableProfiles <- estuaryCombined %>%
  filter(!is.na(profile)) %>%
  distinct(profile = trimws(tolower(as.character(profile)))) %>%
  arrange(profile) %>%
  pull(profile)

missingSelectedProfiles <- setdiff(
  strProfile[!strProfile %in% c("all", "combined")],
  availableProfiles
)

if (length(missingSelectedProfiles) > 0) {
  stop(
    "Selected profiles were not found: ",
    paste(missingSelectedProfiles, collapse = ", "),
    ". Available profiles: ",
    paste(c("all", "combined", availableProfiles), collapse = ", ")
  )
}

dir.create(strImagePath, recursive = TRUE, showWarnings = FALSE)

strPlotStackPath <- file.path(strImagePath, "zPlotStacks")
dir.create(strPlotStackPath, recursive = TRUE, showWarnings = FALSE)

strPCAOutputName <- file.path(strImagePath, "temperatureWindowPCAData.csv")

if (file.exists(strPCAOutputName)) {
  file.remove(strPCAOutputName)
}

bolPCAHeaderWritten <- FALSE
intPCARowsWritten <- 0L

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
  mutate(
    profileGroup = ifelse(
      bolCombineDeepAndBottom &
        tolower(profileGroup) %in% c("deep", "bottom"),
      "bottom",
      profileGroup
    )
  ) %>%
  filter(
    selectorMatches(regionGroup, strRegion),
    selectorMatches(estuaryGroup, strEstuary),
    selectorMatches(stationGroup, strStation),
    selectorMatches(profileGroup, strProfile),
    selectorMatches(seasonGroup, strSeason),
    tolower(estuaryGroup) != "not recorded",
    !is.na(DateTime),
    is.finite(raw_h2otemp)
  ) %>%
  mutate(
    regionGroup = if (selectorIs(strRegion, "combined")) "Combined" else regionGroup,
    estuaryGroup = if (selectorIs(strEstuary, "combined")) "Combined" else estuaryGroup,
    stationGroup = if (selectorIs(strStation, "combined")) "Combined" else stationGroup,
    profileGroup = if (selectorIs(strProfile, "combined")) "Combined" else profileGroup,
    seasonGroup = if (selectorIs(strSeason, "combined")) "Combined" else seasonGroup
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

effectiveSelectedProfiles <- strProfile
if (bolCombineDeepAndBottom) {
  effectiveSelectedProfiles[effectiveSelectedProfiles == "deep"] <- "bottom"
  effectiveSelectedProfiles <- unique(effectiveSelectedProfiles)
}

bolDetectSingleDepth <-
  selectorIs(strProfile, "all") ||
  all(c("bottom", "surface") %in% effectiveSelectedProfiles)

if (bolDetectSingleDepth) {
  dfSingleDepthEstuaries <- dfSelectedProfile %>%
    distinct(regionGroup, estuaryGroup, profileGroup) %>%
    group_by(regionGroup, estuaryGroup) %>%
    summarise(
      hasBottom = any(tolower(profileGroup) == "bottom"),
      hasSurface = any(tolower(profileGroup) == "surface"),
      .groups = "drop"
    ) %>%
    filter(xor(hasBottom, hasSurface)) %>%
    mutate(
      originalProfile = ifelse(hasBottom, "bottom", "surface")
    )

  if (nrow(dfSingleDepthEstuaries) > 0) {
    for (singleDepthIndex in seq_len(nrow(dfSingleDepthEstuaries))) {
      idxSingleDepth <-
        dfSelectedProfile$regionGroup ==
          dfSingleDepthEstuaries$regionGroup[singleDepthIndex] &
        dfSelectedProfile$estuaryGroup ==
          dfSingleDepthEstuaries$estuaryGroup[singleDepthIndex] &
        tolower(dfSelectedProfile$profileGroup) ==
          dfSingleDepthEstuaries$originalProfile[singleDepthIndex]

      dfSelectedProfile$profileGroup[idxSingleDepth] <- "single depth"
    }

    cat(
      "\nRelabeled the only surface/bottom profile as single depth for ",
      nrow(dfSingleDepthEstuaries),
      " estuaries:\n",
      sep = ""
    )
    print(
      as.data.frame(
        dfSingleDepthEstuaries %>%
          select(regionGroup, estuaryGroup, originalProfile)
      ),
      row.names = FALSE
    )
  } else {
    cat("\nNo surface-only or bottom-only estuaries required single-depth relabeling.\n")
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
  all(c(
    selectorIs(strRegion, "all"),
    selectorIs(strEstuary, "all"),
    selectorIs(strStation, "all"),
    selectorIs(strProfile, "all"),
    selectorIs(strSeason, "all")
  )) &&
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
  "Qualifying window summaries will also be appended to temperatureWindowPCAData.csv.\n",
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

  dfWindowOutput <- dfWindowSummary %>%
    mutate(
      region = strGroupRegion,
      estuary = strGroupEstuary,
      station = strGroupStation,
      profile = strGroupProfile,
      season = strGroupSeason,
      .before = 1
    ) %>%
    select(
      region,
      estuary,
      station,
      profile,
      season,
      windowCenterDateTime,
      observationCount,
      minTemp,
      meanTemp,
      medianTemp,
      maxTemp,
      tempRange
    )

  write.csv(
    dfWindowOutput,
    file.path(
      strEstuaryFolder,
      "temperatureSummary.csv"
    ),
    row.names = FALSE
  )

  write.table(
    dfWindowOutput,
    file = strPCAOutputName,
    sep = ",",
    row.names = FALSE,
    col.names = !bolPCAHeaderWritten,
    append = bolPCAHeaderWritten,
    quote = TRUE,
    na = ""
  )

  bolPCAHeaderWritten <- TRUE
  intPCARowsWritten <- intPCARowsWritten + nrow(dfWindowOutput)

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
    geom_freqpoly(
      binwidth = dblTemperatureRangeBinWidth,
      boundary = 0,
      linewidth = 0.8,
      color = "steelblue"
    ) +
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
    geom_freqpoly(
      binwidth = dblTemperatureBinWidth,
      boundary = 0,
      linewidth = 0.8,
      color = "steelblue"
    ) +
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
    dfWindowOutput,
    plotMeanTimeSeries,
    plotRangeFrequencyPolygon,
    plotMeanFrequencyPolygon,
    plotTimeSeriesRangeMeanStack
  )
  gc()
}

cat("Finished writing profile histograms to: ", strImagePath, "\n", sep = "")

if (bolPCAHeaderWritten) {
  cat(
    "Wrote ",
    format(intPCARowsWritten, big.mark = ","),
    " PCA-ready window rows to: ",
    strPCAOutputName,
    "\n",
    sep = ""
  )
} else {
  cat("No groups passed the cutoff, so no PCA-ready CSV was written.\n")
}

gc()
