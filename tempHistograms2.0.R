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

### Select "all" or one profile: "bottom", "surface", "deep", or "mid".
strProfile <- "surface"

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
    scale_x_continuous(
      limits = if (strColumnName == "tempRange") {
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
        " Histogram, Profile = ",
        strSelectedProfile
      ),
      subtitle = paste0(
        "Windows centered at 12:00 noon ",
        strWindowTimeZone,
        ", n = ",
        format(nrow(dfPlot), big.mark = ","),
        " windows"
      ),
      x = strXAxisLabel,
      y = "Number of Days"
    ) +
    theme_minimal()

  strPlotFilename <- paste0(
    makeSafeFilename(strEstuary),
    "_",
    makeSafeFilename(strSelectedProfile),
    "_26HourNoonCentered_",
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
        " Time Series, Profile = ",
        strSelectedProfile
      ),
      subtitle = paste0(
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
    makeSafeFilename(strEstuary),
    "_",
    makeSafeFilename(strSelectedProfile),
    "_26HourNoonCentered_",
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

requiredFields <- c("estuaryname", "profile", "DateTime", "raw_h2otemp")
missingFields <- setdiff(requiredFields, names(estuaryCombined))

if (length(missingFields) > 0) {
  stop("Input dataset is missing required fields: ", paste(missingFields, collapse = ", "))
}

strProfile <- trimws(tolower(strProfile))

availableProfiles <- estuaryCombined %>%
  filter(!is.na(profile)) %>%
  distinct(profile = trimws(tolower(as.character(profile)))) %>%
  arrange(profile) %>%
  pull(profile)

if (strProfile != "all" && !strProfile %in% availableProfiles) {
  stop(
    "Selected profile was not found: ",
    strProfile,
    ". Available profiles: ",
    paste(c("all", availableProfiles), collapse = ", ")
  )
}

dir.create(strImagePath, recursive = TRUE, showWarnings = FALSE)

strPlotStackPath <- file.path(strImagePath, "zPlotStacks")
dir.create(strPlotStackPath, recursive = TRUE, showWarnings = FALSE)

############################################################
### Prepare Selected Profile Data
############################################################

dfSelectedProfile <- estuaryCombined %>%
  mutate(
    profileClean = trimws(tolower(as.character(profile))),
    raw_h2otemp = suppressWarnings(as.numeric(raw_h2otemp))
  ) %>%
  filter(
    strProfile == "all" | profileClean == strProfile,
    !is.na(estuaryname),
    trimws(estuaryname) != "",
    !is.na(DateTime),
    is.finite(raw_h2otemp)
  )

if (nrow(dfSelectedProfile) == 0) {
  stop("No usable temperature observations were found for profile: ", strProfile)
}

temperatureLimits <- range(dfSelectedProfile$raw_h2otemp, na.rm = TRUE)
temperatureRangeLimits <- c(0, diff(temperatureLimits))

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

listEstuaries <- sort(unique(dfSelectedProfile$estuaryname))

cat(
  "Creating 26-hour noon-centered temperature histograms for profile '",
  strProfile,
  "' in ",
  length(listEstuaries),
  " estuaries.\n",
  sep = ""
)

############################################################
### Summarize and Plot Each Estuary
############################################################

for (i in seq_along(listEstuaries)) {
  strEstuary <- listEstuaries[i]
  strFolderName <- paste0(
    makeSafeFilename(strEstuary),
    "_Profile_",
    makeSafeFilename(strProfile)
  )
  strEstuaryFolder <- file.path(strImagePath, strFolderName)

  dir.create(strEstuaryFolder, recursive = TRUE, showWarnings = FALSE)

  cat(
    "Processing estuary ",
    i,
    " of ",
    length(listEstuaries),
    ": ",
    strEstuary,
    "\n",
    sep = ""
  )

  dfEstuary <- dfSelectedProfile %>%
    filter(estuaryname == strEstuary) %>%
    mutate(
      observationId = row_number(),
      observationDate = as.Date(DateTime, tz = strWindowTimeZone),
      baseNoon = as.POSIXct(
        paste(observationDate, "12:00:00"),
        tz = strWindowTimeZone
      )
    )

  ### Test the nearest noon plus the noon before and after it.
  dfWindowObservations <- bind_rows(
    dfEstuary %>% mutate(windowCenterDateTime = baseNoon - 24 * 60 * 60),
    dfEstuary %>% mutate(windowCenterDateTime = baseNoon),
    dfEstuary %>% mutate(windowCenterDateTime = baseNoon + 24 * 60 * 60)
  ) %>%
    filter(
      DateTime >= windowCenterDateTime - 13 * 60 * 60,
      DateTime <= windowCenterDateTime + 13 * 60 * 60
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
      paste0(
        makeSafeFilename(strEstuary),
        "_",
        makeSafeFilename(strProfile),
        "_26Hour_Noon_Centered_Temperature_Summary.csv"
      )
    ),
    row.names = FALSE
  )

  saveSummaryHistogram(
    dfWindowSummary,
    "maxTemp",
    "Maximum Temperature",
    "26-Hour Window Maximum Water Temperature (degrees C)",
    strEstuary,
    strProfile,
    strEstuaryFolder
  )

  saveSummaryHistogram(
    dfWindowSummary,
    "meanTemp",
    "Mean Temperature",
    "26-Hour Window Mean Water Temperature (degrees C)",
    strEstuary,
    strProfile,
    strEstuaryFolder
  )

  saveSummaryHistogram(
    dfWindowSummary,
    "minTemp",
    "Minimum Temperature",
    "26-Hour Window Minimum Water Temperature (degrees C)",
    strEstuary,
    strProfile,
    strEstuaryFolder
  )

  saveSummaryHistogram(
    dfWindowSummary,
    "tempRange",
    "Temperature Range",
    "26-Hour Window Temperature Range (degrees C)",
    strEstuary,
    strProfile,
    strEstuaryFolder
  )

  saveSummaryTimeSeries(
    dfWindowSummary,
    "maxTemp",
    "Maximum Temperature",
    "Maximum Water Temperature (degrees C)",
    strEstuary,
    strProfile,
    strEstuaryFolder
  )

  saveSummaryTimeSeries(
    dfWindowSummary,
    "meanTemp",
    "Mean Temperature",
    "Mean Water Temperature (degrees C)",
    strEstuary,
    strProfile,
    strEstuaryFolder
  )

  saveSummaryTimeSeries(
    dfWindowSummary,
    "minTemp",
    "Minimum Temperature",
    "Minimum Water Temperature (degrees C)",
    strEstuary,
    strProfile,
    strEstuaryFolder
  )

  saveSummaryTimeSeries(
    dfWindowSummary,
    "tempRange",
    "Temperature Range",
    "Temperature Range (degrees C)",
    strEstuary,
    strProfile,
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
        strEstuary,
        ": Median Temperature Time Series, Profile = ",
        strProfile
      ),
      subtitle = paste0(
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
    scale_x_continuous(limits = temperatureRangeLimits) +
    labs(
      title = paste0(strEstuary, ": Temperature Range Histogram, Profile = ", strProfile),
      subtitle = paste0(
        "26-hour windows, n = ",
        format(nrow(dfWindowSummary), big.mark = ",")
      ),
      x = "26-Hour Window Temperature Range (degrees C)",
      y = "Number of Windows"
    ) +
    theme_minimal()

  plotMedianHistogram <- ggplot(dfWindowSummary, aes(x = medianTemp)) +
    geom_histogram(bins = intHistogramBins, color = "white", fill = "steelblue") +
    scale_x_continuous(limits = temperatureLimits) +
    labs(
      title = paste0(strEstuary, ": Median Temperature Histogram, Profile = ", strProfile),
      subtitle = paste0(
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

  strStackFilename <- paste0(
    makeSafeFilename(strEstuary),
    "_",
    makeSafeFilename(strProfile),
    "_Median_TimeSeries_Range_Median_Histogram_Stack.png"
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
    to = file.path(strPlotStackPath, strStackFilename),
    overwrite = TRUE
  )

  rm(dfEstuary, dfWindowObservations, dfWindowSummary)
  gc()
}

cat("Finished writing profile histograms to: ", strImagePath, "\n", sep = "")

gc()
