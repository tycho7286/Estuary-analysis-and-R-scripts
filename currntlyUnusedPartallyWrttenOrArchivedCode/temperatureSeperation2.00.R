## Import and Load Packages

# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("ggrastr")
# install.packages("patchwork")

library(dplyr)
library(ggplot2)
library(ggrastr)
library(patchwork)

# # Windows
# strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataCombined"
# strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
# strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/plotsAndImages/tempHistograms"

# Linux
strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
strImagePath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/plotsAndImages/tempHistograms"

strReadFilename <- "dataWorkingCopy.rds"
strFullName <- file.path(strInPath, strReadFilename)

estuaryCombined <- readRDS(strFullName)

dir.create(strImagePath, recursive = TRUE, showWarnings = FALSE)

listEstuaryList <- unique(estuaryCombined$estuaryname)
print(listEstuaryList)

cat("The following", length(listEstuaryList), "estuaries will be processed:\n")
print(listEstuaryList)

# strEstuary <- "Noyo River"
# strEstuary <- unique(estuaryCombined$estuaryname)[5]
# strProject <- "State-Parks"
intBinWidth <- 0.5

getMaxPct <- function(df, breaks) {
  if (nrow(df) == 0) {
    return(0)
  }
  h <- hist(df$raw_h2otemp, breaks = breaks, plot = FALSE)
  max(h$counts / sum(h$counts) * 100, na.rm = TRUE)
}

makeWindowDataList <- function(dfTempFull, yearsToPlot) {
  windowDataList <- list()
  
  for (plotYear in yearsToPlot) {
    
    yearStart <- as.POSIXct(paste0(plotYear, "-01-01"), tz = "UTC")
    yearEnd <- as.POSIXct(paste0(plotYear + 1, "-01-01"), tz = "UTC")
    
    dfYear <- dfTempFull %>% filter(DateTime >= yearStart, DateTime < yearEnd)
    
    dfJanMar <- dfYear %>% filter(DateTime >= as.POSIXct(paste0(plotYear, "-01-01"), tz = "UTC"), DateTime < as.POSIXct(paste0(plotYear, "-04-01"), tz = "UTC"))
    dfAprJun <- dfYear %>% filter(DateTime >= as.POSIXct(paste0(plotYear, "-04-01"), tz = "UTC"), DateTime < as.POSIXct(paste0(plotYear, "-07-01"), tz = "UTC"))
    dfJulSep <- dfYear %>% filter(DateTime >= as.POSIXct(paste0(plotYear, "-07-01"), tz = "UTC"), DateTime < as.POSIXct(paste0(plotYear, "-10-01"), tz = "UTC"))
    dfOctDec <- dfYear %>% filter(DateTime >= as.POSIXct(paste0(plotYear, "-10-01"), tz = "UTC"), DateTime < as.POSIXct(paste0(plotYear + 1, "-01-01"), tz = "UTC"))
    
    windowDataList[[paste0(plotYear, "_Full")]] <- dfYear
    windowDataList[[paste0(plotYear, "_JanMar")]] <- dfJanMar
    windowDataList[[paste0(plotYear, "_AprJun")]] <- dfAprJun
    windowDataList[[paste0(plotYear, "_JulSep")]] <- dfJulSep
    windowDataList[[paste0(plotYear, "_OctDec")]] <- dfOctDec
  }
  
  windowDataList
}

for (i in seq_along(listEstuaryList)) {
  
  strEstuary <- listEstuaryList[i]
  
  strProjects <- paste(
    sort(unique(
      estuaryCombined$projectid[
        estuaryCombined$estuaryname == strEstuary
      ]
    )),
    collapse = ", "
  )
  
  cat(
    "\n====================================================\n",
    "Processing estuary ", i, " of ", length(listEstuaryList), ": ",
    strEstuary, "\n",
    "Project(s): ", strProjects, "\n",
    "====================================================\n",
    sep = ""
  )
  
  dfTempFull <- estuaryCombined %>%
    filter(estuaryname == strEstuary, !is.na(raw_h2otemp), !is.na(DateTime))
  
  if (nrow(dfTempFull) == 0) {
    cat("No temperature data found for ", strEstuary, ". Skipping.\n", sep = "")
    next
  }
  
  yearsToPlot <- sort(unique(as.numeric(format(dfTempFull$DateTime, "%Y"))))
  
  xLimits <- range(dfTempFull$raw_h2otemp, na.rm = TRUE)
  
  breaks <- seq(
    floor(min(dfTempFull$raw_h2otemp, na.rm = TRUE)),
    ceiling(max(dfTempFull$raw_h2otemp, na.rm = TRUE)) + intBinWidth,
    by = intBinWidth
  )
  
  windowDataList <- makeWindowDataList(dfTempFull, yearsToPlot)
  
  maxPercent <- max(sapply(windowDataList, getMaxPct, breaks = breaks), na.rm = TRUE)
  
  makeHistPlot <- function(df, plotTitle) {
    
    intN <- nrow(df)
    
    ggplot(df, aes(x = raw_h2otemp)) +
      geom_histogram(aes(y = after_stat(count / sum(count) * 100)), breaks = breaks) +
      scale_x_continuous(limits = xLimits) +
      scale_y_continuous(limits = c(0, maxPercent)) +
      labs(title = paste(strEstuary, plotTitle), subtitle = paste0("n = ", format(intN, big.mark = ",")), x = "Water Temperature (°C)", y = "Percent") +
      theme_minimal()
  }
  
  makeTimeSeriesPlot <- function(df, plotTitle) {
    
    intN <- nrow(df)
    
    ggplot(df, aes(x = DateTime, y = raw_h2otemp)) +
      geom_line(linewidth = 0.2) +
      labs(title = paste(strEstuary, plotTitle), subtitle = paste0("n = ", format(intN, big.mark = ",")), x = "Date", y = "Water Temperature (°C)") +
      theme_minimal()
  }
  
  makeYearPlot <- function(plotYear) {
    
    yearStart <- as.POSIXct(paste0(plotYear, "-01-01"), tz = "UTC")
    yearEnd <- as.POSIXct(paste0(plotYear + 1, "-01-01"), tz = "UTC")
    
    dfYear <- dfTempFull %>% filter(DateTime >= yearStart, DateTime < yearEnd)
    
    dfJanMar <- dfYear %>% filter(DateTime >= as.POSIXct(paste0(plotYear, "-01-01"), tz = "UTC"), DateTime < as.POSIXct(paste0(plotYear, "-04-01"), tz = "UTC"))
    dfAprJun <- dfYear %>% filter(DateTime >= as.POSIXct(paste0(plotYear, "-04-01"), tz = "UTC"), DateTime < as.POSIXct(paste0(plotYear, "-07-01"), tz = "UTC"))
    dfJulSep <- dfYear %>% filter(DateTime >= as.POSIXct(paste0(plotYear, "-07-01"), tz = "UTC"), DateTime < as.POSIXct(paste0(plotYear, "-10-01"), tz = "UTC"))
    dfOctDec <- dfYear %>% filter(DateTime >= as.POSIXct(paste0(plotYear, "-10-01"), tz = "UTC"), DateTime < as.POSIXct(paste0(plotYear + 1, "-01-01"), tz = "UTC"))
    
    plotYearTimeSeries <- makeTimeSeriesPlot(dfYear, paste0("Temperature Time Series ", plotYear))
    plotYearFull <- makeHistPlot(dfYear, paste0(plotYear, " Full Year Temperature Distribution"))
    plotJanMar <- makeHistPlot(dfJanMar, paste0(plotYear, " Jan-Mar Temperature Distribution"))
    plotAprJun <- makeHistPlot(dfAprJun, paste0(plotYear, " Apr-Jun Temperature Distribution"))
    plotJulSep <- makeHistPlot(dfJulSep, paste0(plotYear, " Jul-Sep Temperature Distribution"))
    plotOctDec <- makeHistPlot(dfOctDec, paste0(plotYear, " Oct-Dec Temperature Distribution"))
    
    plotYearTimeSeries / plotYearFull / plotJanMar / plotAprJun / plotJulSep / plotOctDec
  }
  
  for (plotYear in yearsToPlot) {
    
    yearPlot <- makeYearPlot(plotYear)
    
    ggsave(
      filename = file.path(strImagePath, paste0(gsub(" ", "_", strEstuary), "_", plotYear, "_Temperature.png")),
      plot = yearPlot,
      width = 8,
      height = 18,
      units = "in",
      dpi = 1200,
      bg = "white"
    )
  }
  
  dfJanMarAll <- dfTempFull %>% filter(as.numeric(format(DateTime, "%m")) %in% c(1, 2, 3))
  dfAprJunAll <- dfTempFull %>% filter(as.numeric(format(DateTime, "%m")) %in% c(4, 5, 6))
  dfJulSepAll <- dfTempFull %>% filter(as.numeric(format(DateTime, "%m")) %in% c(7, 8, 9))
  dfOctDecAll <- dfTempFull %>% filter(as.numeric(format(DateTime, "%m")) %in% c(10, 11, 12))
  
  plotAllTimeSeries <- makeTimeSeriesPlot(dfTempFull, "Full Temperature Time Series")
  
  plotJanMarAll <- makeHistPlot(dfJanMarAll, "Jan-Mar Temperature Distribution, All Years")
  plotAprJunAll <- makeHistPlot(dfAprJunAll, "Apr-Jun Temperature Distribution, All Years")
  plotJulSepAll <- makeHistPlot(dfJulSepAll, "Jul-Sep Temperature Distribution, All Years")
  plotOctDecAll <- makeHistPlot(dfOctDecAll, "Oct-Dec Temperature Distribution, All Years")
  
  plotAllMonths <- plotAllTimeSeries / plotJanMarAll / plotAprJunAll / plotJulSepAll / plotOctDecAll
  
  ggsave(
    filename = file.path(strImagePath, paste0(gsub(" ", "_", strEstuary), "_AllYears_MonthGroups_Temperature.png")),
    plot = plotAllMonths,
    width = 8,
    height = 16,
    units = "in",
    dpi = 1200,
    bg = "white"
  )
  
  gc()
}

gc()
