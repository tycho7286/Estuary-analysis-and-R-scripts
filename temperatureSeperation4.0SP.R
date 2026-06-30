## Import and Load Packages

# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("ggrastr")
# install.packages("patchwork")

library(dplyr)
library(ggplot2)
library(ggrastr)
library(patchwork)

# Windows
strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
strImagePath <- "D:/Google/School/2026Summer-BML-UCDGAP/plotsAndImages/tempHistograms"

# # Linux
# strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
# strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
# strImagePath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/plotsAndImages/tempHistograms"

strReadFilename <- "datasetWorkingCopy.rds"
strFullName <- file.path(strInPath, strReadFilename)

estuaryCombined <- readRDS(strFullName)

dir.create(strImagePath, recursive = TRUE, showWarnings = FALSE)


strProject <- "State-Parks"
intBinWidth <- 0.5

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

### Find the coordinate columns.
getCoordinateColumns <- function(df) {
  latitudeNames <- c("latitude", "siteLatitude", "estuaryLatitude", "lat", "Latitude")
  longitudeNames <- c("longitude", "siteLongitude", "estuaryLongitude", "lon", "Longitude")

  latitudeCol <- latitudeNames[latitudeNames %in% names(df)][1]
  longitudeCol <- longitudeNames[longitudeNames %in% names(df)][1]

  if (is.na(latitudeCol) | is.na(longitudeCol)) {
    stop("Could not find latitude and longitude columns. Expected latitude/longitude, siteLatitude/siteLongitude, or estuaryLatitude/estuaryLongitude.")
  }

  list(latitudeCol = latitudeCol, longitudeCol = longitudeCol)
}

### Add month windows from DateTime.
addSeason <- function(df) {
  df %>%
    mutate(
      intMonth = as.numeric(format(DateTime, "%m")),
      season = case_when(
        intMonth %in% c(1, 2, 3) ~ "Jan-Mar",
        intMonth %in% c(4, 5, 6) ~ "Apr-Jun",
        intMonth %in% c(7, 8, 9) ~ "Jul-Sep",
        intMonth %in% c(10, 11, 12) ~ "Oct-Dec",
        TRUE ~ NA_character_
      ),
      season = factor(
        season,
        levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")
      )
    )
}

### Make percent histogram.
makeHistogramPlot <- function(df, breaks, xLimits, maxPercent) {
  ggplot(df, aes(x = raw_h2otemp)) +
    geom_histogram(
      aes(y = after_stat(count / sum(count) * 100)),
      breaks = breaks
    ) +
    scale_x_continuous(limits = xLimits) +
    scale_y_continuous(limits = c(0, maxPercent)) +
    facet_grid(estuarynameOrdered ~ season) +
    labs(
      title = "State Parks Temperature Histograms by Season",
      subtitle = "Estuaries ordered south to north by latitude",
      x = "Water Temperature (°C)",
      y = "Percent"
    ) +
    theme_minimal() +
    theme(
      strip.text.y = element_text(angle = 0, hjust = 0),
      axis.text.y = element_text(size = 7),
      axis.text.x = element_text(size = 7)
    )
}

### Calculate maximum histogram percent for shared y-axis.
getMaxPercent <- function(df, breaks) {
  if (nrow(df) == 0) {
    return(0)
  }

  histList <- df %>%
    group_by(estuarynameOrdered, season) %>%
    summarise(
      histCounts = list(hist(raw_h2otemp, breaks = breaks, plot = FALSE)$counts),
      .groups = "drop"
    )

  maxPercent <- max(
    sapply(
      histList$histCounts,
      function(counts) {
        if (sum(counts) == 0) {
          return(0)
        }
        max(counts / sum(counts) * 100, na.rm = TRUE)
      }
    ),
    na.rm = TRUE
  )

  maxPercent
}

### Make five-number summary data.
makeSummaryData <- function(df) {
  df %>%
    group_by(estuarynameOrdered, season) %>%
    summarise(
      n = n(),
      minTemp = min(raw_h2otemp, na.rm = TRUE),
      q1Temp = quantile(raw_h2otemp, probs = 0.25, na.rm = TRUE),
      medianTemp = median(raw_h2otemp, na.rm = TRUE),
      q3Temp = quantile(raw_h2otemp, probs = 0.75, na.rm = TRUE),
      maxTemp = max(raw_h2otemp, na.rm = TRUE),
      .groups = "drop"
    )
}

### Make one waterfall summary plot.
makeWaterfallSummaryPlot <- function(dfSummary, strColumnName, strPlotTitle) {
  ggplot(dfSummary, aes(x = estuarynameOrdered, y = .data[[strColumnName]], group = season)) +
    geom_line(aes(linetype = season), linewidth = 0.3) +
    geom_point(aes(shape = season), size = 1.3) +
    labs(
      title = strPlotTitle,
      subtitle = "State Parks estuaries ordered south to north by latitude",
      x = "Estuary, south to north",
      y = "Water Temperature (°C)",
      linetype = "Season",
      shape = "Season"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 7)
    )
}

############################################################
### Prepare Data
############################################################

coordinateCols <- getCoordinateColumns(estuaryCombined)

dfTempFull <- estuaryCombined %>%
  filter(
    projectid == strProject,
    !is.na(raw_h2otemp),
    !is.na(DateTime),
    !is.na(.data[[coordinateCols$latitudeCol]]),
    !is.na(.data[[coordinateCols$longitudeCol]])
  ) %>%
  rename(
    latitudeForOrder = all_of(coordinateCols$latitudeCol),
    longitudeForOrder = all_of(coordinateCols$longitudeCol)
  ) %>%
  addSeason() %>%
  filter(!is.na(season))

if (nrow(dfTempFull) == 0) {
  stop("No State Parks temperature records found with DateTime, raw_h2otemp, and coordinates.")
}

dfEstuaryOrder <- dfTempFull %>%
  group_by(estuaryname) %>%
  summarise(
    latitudeForOrder = mean(latitudeForOrder, na.rm = TRUE),
    longitudeForOrder = mean(longitudeForOrder, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(latitudeForOrder)

listEstuaryOrder <- dfEstuaryOrder$estuaryname

dfTempFull <- dfTempFull %>%
  mutate(
    estuarynameOrdered = factor(
      estuaryname,
      levels = listEstuaryOrder
    )
  )

cat("The following", length(listEstuaryOrder), "State Parks estuaries will be processed south to north:\n")
print(listEstuaryOrder)

xLimits <- range(dfTempFull$raw_h2otemp, na.rm = TRUE)

breaks <- seq(
  floor(min(dfTempFull$raw_h2otemp, na.rm = TRUE)),
  ceiling(max(dfTempFull$raw_h2otemp, na.rm = TRUE)) + intBinWidth,
  by = intBinWidth
)

maxPercent <- getMaxPercent(dfTempFull, breaks)

############################################################
### Seasonal Histogram Plot
############################################################

plotSeasonalHistograms <- makeHistogramPlot(
  df = dfTempFull,
  breaks = breaks,
  xLimits = xLimits,
  maxPercent = maxPercent
)

print(plotSeasonalHistograms)

ggsave(
  filename = file.path(strImagePath, "StateParks_Seasonal_Temperature_Histograms_SouthToNorth.png"),
  plot = plotSeasonalHistograms,
  width = 18,
  height = 22,
  units = "in",
  dpi = 600,
  bg = "white"
)

############################################################
### Five-Number Summary Waterfall Plots
############################################################

dfSummary <- makeSummaryData(dfTempFull)

plotMin <- makeWaterfallSummaryPlot(
  dfSummary = dfSummary,
  strColumnName = "minTemp",
  strPlotTitle = "State Parks Seasonal Minimum Temperature"
)

plotQ1 <- makeWaterfallSummaryPlot(
  dfSummary = dfSummary,
  strColumnName = "q1Temp",
  strPlotTitle = "State Parks Seasonal First Quartile Temperature"
)

plotMedian <- makeWaterfallSummaryPlot(
  dfSummary = dfSummary,
  strColumnName = "medianTemp",
  strPlotTitle = "State Parks Seasonal Median Temperature"
)

plotQ3 <- makeWaterfallSummaryPlot(
  dfSummary = dfSummary,
  strColumnName = "q3Temp",
  strPlotTitle = "State Parks Seasonal Third Quartile Temperature"
)

plotMax <- makeWaterfallSummaryPlot(
  dfSummary = dfSummary,
  strColumnName = "maxTemp",
  strPlotTitle = "State Parks Seasonal Maximum Temperature"
)

plotFiveNumberSummary <- plotMin / plotQ1 / plotMedian / plotQ3 / plotMax

print(plotFiveNumberSummary)

ggsave(
  filename = file.path(strImagePath, "StateParks_FiveNumberSummary_Waterfall_SouthToNorth.png"),
  plot = plotFiveNumberSummary,
  width = 18,
  height = 24,
  units = "in",
  dpi = 600,
  bg = "white"
)

############################################################
### Export Individual Summary Plots
############################################################

ggsave(
  filename = file.path(strImagePath, "StateParks_Minimum_Temperature_Waterfall_SouthToNorth.png"),
  plot = plotMin,
  width = 18,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = file.path(strImagePath, "StateParks_Q1_Temperature_Waterfall_SouthToNorth.png"),
  plot = plotQ1,
  width = 18,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = file.path(strImagePath, "StateParks_Median_Temperature_Waterfall_SouthToNorth.png"),
  plot = plotMedian,
  width = 18,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = file.path(strImagePath, "StateParks_Q3_Temperature_Waterfall_SouthToNorth.png"),
  plot = plotQ3,
  width = 18,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = file.path(strImagePath, "StateParks_Maximum_Temperature_Waterfall_SouthToNorth.png"),
  plot = plotMax,
  width = 18,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)

gc()
