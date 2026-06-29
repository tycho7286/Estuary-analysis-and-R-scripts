### install and load packages
#install.packages("dplyr")
#install.packages("gsw")
#install.packages("oce")
#install.packages("ggplot2")
#install.packages("ggrastr")
#install.packages("patchwork")

library(dplyr)
library(gsw)
library(oce)
library(ggplot2)
library(ggrastr)
library(patchwork)

### load in variables and read datafile
# # Windows
# strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataCombined"
# strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"

# Linux
strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataCombined"
strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"

strReadFilename <- "combinedDataset.rds"
strWriteFilename <- "datasetWorkingCopy.rds"
strFullName <- file.path(strInPath, strReadFilename)
### Read combined dataset
#estuaryCombined <- readRDS(strFullName)


strEstuary <- "Noyo River"
strProject <- "State-Parks"
intBinWidth <- 0.5

timeWindowOneStart <- as.POSIXct("2024-01-01", tz = "UTC")
timeWindowOneEnd <- as.POSIXct("2024-12-31", tz = "UTC")

timeWindowTwoStart <- as.POSIXct("2025-01-01", tz = "UTC")
timeWindowTwoEnd <- as.POSIXct("2025-12-31", tz = "UTC")

timeWindowThreeStart <- as.POSIXct("2026-01-01", tz = "UTC")
timeWindowThreeEnd <- as.POSIXct("2026-12-31", tz = "UTC")

dfTempFull <- estuaryCombined %>%
  filter(estuaryname == strEstuary, projectid == strProject, !is.na(raw_h2otemp), !is.na(DateTime))

dfTempWindowOne <- estuaryCombined %>%
  filter(estuaryname == strEstuary, projectid == strProject, DateTime >= timeWindowOneStart, DateTime < timeWindowOneEnd, !is.na(raw_h2otemp), !is.na(DateTime))

dfTempWindowTwo <- estuaryCombined %>%
  filter(estuaryname == strEstuary, projectid == strProject, DateTime >= timeWindowTwoStart, DateTime < timeWindowTwoEnd, !is.na(raw_h2otemp), !is.na(DateTime))

dfTempWindowThree <- estuaryCombined %>%
  filter(estuaryname == strEstuary, projectid == strProject, DateTime >= timeWindowThreeStart, DateTime < timeWindowThreeEnd, !is.na(raw_h2otemp), !is.na(DateTime))

strFullRange <- paste0(format(min(dfTempFull$DateTime, na.rm = TRUE), "%Y-%b"), " - ", format(max(dfTempFull$DateTime, na.rm = TRUE), "%Y-%b"))

xLimits <- range(c(dfTempFull$raw_h2otemp, dfTempWindowOne$raw_h2otemp, dfTempWindowTwo$raw_h2otemp, dfTempWindowThree$raw_h2otemp), na.rm = TRUE)

getMaxPct <- function(df, binWidth) {
  breaks <- seq(floor(min(df$raw_h2otemp, na.rm = TRUE)), ceiling(max(df$raw_h2otemp, na.rm = TRUE)) + binWidth, by = binWidth)
  h <- hist(df$raw_h2otemp, breaks = breaks, plot = FALSE)
  max(h$counts / sum(h$counts) * 100, na.rm = TRUE)
}

maxPercent <- max(
  getMaxPct(dfTempFull, intBinWidth),
  getMaxPct(dfTempWindowOne, intBinWidth),
  getMaxPct(dfTempWindowTwo, intBinWidth),
  getMaxPct(dfTempWindowThree, intBinWidth)
)

plotTimeSeries <- ggplot(dfTempFull, aes(x = DateTime, y = raw_h2otemp)) +
  geom_line(linewidth = 0.2) +
  labs(
    title = paste0(
      strEstuary,
      " Temperature Time Series (",
      strFullRange,
      ")"
    ),
    x = "Date",
    y = "Water Temperature (°C)"
  ) +
  theme_minimal()
plotTimeSeries

plotFull <- ggplot(dfTempFull, aes(x = raw_h2otemp)) +
  geom_histogram(aes(y = after_stat(count / sum(count) * 100)), binwidth = intBinWidth) +
  scale_x_continuous(limits = xLimits) +
  scale_y_continuous(limits = c(0, maxPercent)) +
  labs(title = paste0(strEstuary, " Temperature Distribution (", strFullRange, ")"), x = "Water Temperature (°C)", y = "Percent") +
  theme_minimal()

plotWindowOne <- ggplot(dfTempWindowOne, aes(x = raw_h2otemp)) +
  geom_histogram(aes(y = after_stat(count / sum(count) * 100)), binwidth = intBinWidth) +
  scale_x_continuous(limits = xLimits) +
  scale_y_continuous(limits = c(0, maxPercent)) +
  labs(title = paste0(format(timeWindowOneStart, "%Y-%b"), " - ", format(timeWindowOneEnd, "%Y-%b")), x = "Water Temperature (°C)", y = "Percent") +
  theme_minimal()

plotWindowTwo <- ggplot(dfTempWindowTwo, aes(x = raw_h2otemp)) +
  geom_histogram(aes(y = after_stat(count / sum(count) * 100)), binwidth = intBinWidth) +
  scale_x_continuous(limits = xLimits) +
  scale_y_continuous(limits = c(0, maxPercent)) +
  labs(title = paste0(format(timeWindowTwoStart, "%Y-%b"), " - ", format(timeWindowTwoEnd, "%Y-%b")), x = "Water Temperature (°C)", y = "Percent") +
  theme_minimal()

plotWindowThree <- ggplot(dfTempWindowThree, aes(x = raw_h2otemp)) +
  geom_histogram(aes(y = after_stat(count / sum(count) * 100)), binwidth = intBinWidth) +
  scale_x_continuous(limits = xLimits) +
  scale_y_continuous(limits = c(0, maxPercent)) +
  labs(title = paste0(format(timeWindowThreeStart, "%Y-%b"), " - ", format(timeWindowThreeEnd, "%Y-%b")), x = "Water Temperature (°C)", y = "Percent") +
  theme_minimal()

plotTimeSeries / plotFull / plotWindowOne / plotWindowTwo / plotWindowThree






