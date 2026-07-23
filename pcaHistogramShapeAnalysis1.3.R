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
strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/plotsAndImages/rStudioPlotOutputs"
strOutPath <- file.path(strInPath, "pcaHistogramShapeAnalysis")

# # Linux
# strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/plotsAndImages/rStudioPlotOutputs"
# strOutPath <- file.path(strInPath, "pcaHistogramShapeAnalysis")

strHistogramPCAFilename <- "histogramPCAData.csv"

### Number of closest histogram-shape matches reported for every group.
intNearestMatches <- 5

intImageWidth <- 10
intImageHeight <- 7
intImageDpi <- 600

############################################################
### Helper Functions
############################################################

makeGroupLabel <- function(region, estuary, station, profile, season) {
  paste(
    region,
    estuary,
    paste0("Station ", station),
    profile,
    season,
    sep = " | "
  )
}

analyzeHistogramShapes <- function(
  strInputFilename,
  strStatistic,
  strAnalysisFolder,
  strMetricTitle,
  strXAxisTitle
) {
  strFullReadName <- file.path(strInPath, strInputFilename)
  strFullOutputPath <- file.path(strOutPath, strAnalysisFolder)

  if (!file.exists(strFullReadName)) {
    stop("Input histogram file does not exist: ", strFullReadName)
  }

  dir.create(strFullOutputPath, recursive = TRUE, showWarnings = FALSE)

  dfHistogramInput <- read.csv(
    strFullReadName,
    stringsAsFactors = FALSE
  )

  groupFields <- c("region", "estuary", "station", "profile", "season")
  requiredFields <- c(
    groupFields,
    "binLower",
    "binUpper",
    "binCenter",
    "count",
    "proportion",
    "statistic"
  )
  missingFields <- setdiff(requiredFields, names(dfHistogramInput))

  if (length(missingFields) > 0) {
    stop(
      strInputFilename,
      " is missing required fields: ",
      paste(missingFields, collapse = ", ")
    )
  }

  dfHistogramInput$statistic <- trimws(
    tolower(as.character(dfHistogramInput$statistic))
  )
  dfHistogramInput <- dfHistogramInput %>%
    filter(statistic == strStatistic)

  if (nrow(dfHistogramInput) == 0) {
    stop(
      "No histogram rows were found for statistic: ",
      strStatistic
    )
  }

  for (groupField in groupFields) {
    dfHistogramInput[[groupField]] <- trimws(
      as.character(dfHistogramInput[[groupField]])
    )
    dfHistogramInput[[groupField]][
      is.na(dfHistogramInput[[groupField]]) |
        dfHistogramInput[[groupField]] == ""
    ] <- "Not Recorded"
  }

  numericFields <- c(
    "binLower",
    "binUpper",
    "binCenter",
    "count",
    "proportion"
  )
  for (numericField in numericFields) {
    dfHistogramInput[[numericField]] <- suppressWarnings(
      as.numeric(dfHistogramInput[[numericField]])
    )
  }

  dfHistogramInput <- dfHistogramInput %>%
    filter(
      is.finite(binLower),
      is.finite(binUpper),
      is.finite(binCenter),
      is.finite(count),
      is.finite(proportion)
    ) %>%
    mutate(
      groupKey = paste(
        region,
        estuary,
        station,
        profile,
        season,
        sep = "\r"
      ),
      groupLabel = makeGroupLabel(
        region,
        estuary,
        station,
        profile,
        season
      )
    )

  dfGroupMetadata <- dfHistogramInput %>%
    distinct(
      groupKey,
      region,
      estuary,
      station,
      profile,
      season,
      groupLabel
    ) %>%
    arrange(region, estuary, station, profile, season)

  if (nrow(dfGroupMetadata) < 4) {
    stop(
      strMetricTitle,
      " PCA requires at least four histogram groups to produce PC1-PC3."
    )
  }

  vectorBinCenters <- sort(unique(dfHistogramInput$binCenter))
  matrixHistogramProportions <- matrix(
    0,
    nrow = nrow(dfGroupMetadata),
    ncol = length(vectorBinCenters),
    dimnames = list(dfGroupMetadata$groupLabel, vectorBinCenters)
  )

  for (groupIndex in seq_len(nrow(dfGroupMetadata))) {
    dfCurrentGroup <- dfHistogramInput %>%
      filter(groupKey == dfGroupMetadata$groupKey[groupIndex])

    matchedBins <- match(dfCurrentGroup$binCenter, vectorBinCenters)
    matrixHistogramProportions[groupIndex, matchedBins] <-
      dfCurrentGroup$proportion
  }

  histogramRowSums <- rowSums(matrixHistogramProportions)
  if (any(histogramRowSums <= 0)) {
    stop(strMetricTitle, " contains a histogram with no observations.")
  }

  matrixHistogramProportions <-
    matrixHistogramProportions / histogramRowSums

  write.csv(
    cbind(
      as.data.frame(dfGroupMetadata),
      as.data.frame(matrixHistogramProportions, check.names = FALSE)
    ),
    file.path(strFullOutputPath, "histogramBinProportionsWide.csv"),
    row.names = FALSE
  )

  ############################################################
  ### Direct Histogram Similarity
  ############################################################

  matrixHellingerDistance <-
    as.matrix(dist(sqrt(matrixHistogramProportions))) / sqrt(2)
  matrixHellingerSimilarity <- 1 - matrixHellingerDistance
  pairIndexes <- which(upper.tri(matrixHellingerDistance), arr.ind = TRUE)

  dfShapePairs <- data.frame(
    firstGroup = dfGroupMetadata$groupLabel[pairIndexes[, 1]],
    secondGroup = dfGroupMetadata$groupLabel[pairIndexes[, 2]],
    hellingerDistance = matrixHellingerDistance[pairIndexes],
    hellingerSimilarity = matrixHellingerSimilarity[pairIndexes],
    stringsAsFactors = FALSE
  ) %>%
    arrange(hellingerDistance)

  write.csv(
    dfShapePairs,
    file.path(strFullOutputPath, "histogramShapeAllPairSimilarities.csv"),
    row.names = FALSE
  )

  listNearestMatches <- vector("list", nrow(dfGroupMetadata))

  for (groupIndex in seq_len(nrow(dfGroupMetadata))) {
    orderedMatches <- order(matrixHellingerDistance[groupIndex, ])
    orderedMatches <- orderedMatches[orderedMatches != groupIndex]
    orderedMatches <- head(orderedMatches, intNearestMatches)

    listNearestMatches[[groupIndex]] <- data.frame(
      group = dfGroupMetadata$groupLabel[groupIndex],
      matchRank = seq_along(orderedMatches),
      matchingGroup = dfGroupMetadata$groupLabel[orderedMatches],
      hellingerDistance =
        matrixHellingerDistance[groupIndex, orderedMatches],
      hellingerSimilarity =
        matrixHellingerSimilarity[groupIndex, orderedMatches],
      stringsAsFactors = FALSE
    )
  }

  write.csv(
    bind_rows(listNearestMatches),
    file.path(strFullOutputPath, "histogramShapeNearestMatches.csv"),
    row.names = FALSE
  )

  ############################################################
  ### PCA
  ############################################################

  vectorVariableBins <- apply(matrixHistogramProportions, 2, sd) > 0

  if (sum(vectorVariableBins) < 3) {
    stop(
      strMetricTitle,
      " PCA requires at least three histogram bins with variation."
    )
  }

  resultPCA <- prcomp(
    matrixHistogramProportions[
      ,
      vectorVariableBins,
      drop = FALSE
    ],
    center = TRUE,
    scale. = FALSE
  )

  vectorExplainedVariance <- resultPCA$sdev^2 /
    sum(resultPCA$sdev^2)
  intLoadingComponents <- min(3, ncol(resultPCA$rotation))

  dfPCAVariance <- data.frame(
    component = paste0("PC", seq_along(vectorExplainedVariance)),
    explainedVariance = vectorExplainedVariance,
    cumulativeVariance = cumsum(vectorExplainedVariance)
  )
  dfPCAScores <- cbind(
    as.data.frame(dfGroupMetadata),
    as.data.frame(resultPCA$x)
  )
  dfPCALoadings <- data.frame(
    binCenter = rep(
      vectorBinCenters[vectorVariableBins],
      times = intLoadingComponents
    ),
    component = rep(
      paste0("PC", seq_len(intLoadingComponents)),
      each = sum(vectorVariableBins)
    ),
    loading = as.vector(
      resultPCA$rotation[
        ,
        seq_len(intLoadingComponents),
        drop = FALSE
      ]
    )
  )

  write.csv(
    dfPCAVariance,
    file.path(strFullOutputPath, "histogramShapePCAVariance.csv"),
    row.names = FALSE
  )
  write.csv(
    dfPCAScores,
    file.path(strFullOutputPath, "histogramShapePCAScores.csv"),
    row.names = FALSE
  )
  write.csv(
    dfPCALoadings,
    file.path(strFullOutputPath, "histogramShapePCALoadings.csv"),
    row.names = FALSE
  )

  ############################################################
  ### PCA Plots
  ############################################################

  plotPCAScores <- ggplot(
    dfPCAScores,
    aes(x = PC1, y = PC2, color = region, shape = profile)
  ) +
    geom_point(size = 2.2, alpha = 0.8) +
    labs(
      title = paste0(strMetricTitle, " Histogram Shape PCA"),
      subtitle = "PCA uses normalized histogram-bin proportions",
      x = paste0(
        "PC1 (",
        round(vectorExplainedVariance[1] * 100, 1),
        "%)"
      ),
      y = paste0(
        "PC2 (",
        round(vectorExplainedVariance[2] * 100, 1),
        "%)"
      ),
      color = "Region",
      shape = "Profile"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(strFullOutputPath, "histogramShapePCA.png"),
    plot = plotPCAScores,
    width = intImageWidth,
    height = intImageHeight,
    units = "in",
    dpi = intImageDpi,
    bg = "white"
  )

  listLoadingPlots <- vector("list", intLoadingComponents)
  inferredBinWidth <- min(diff(vectorBinCenters))

  for (componentIndex in seq_len(intLoadingComponents)) {
    componentName <- paste0("PC", componentIndex)
    dfCurrentLoadings <- dfPCALoadings %>%
      filter(component == componentName)

    listLoadingPlots[[componentIndex]] <- ggplot(
      dfCurrentLoadings,
      aes(x = binCenter, y = loading)
    ) +
      geom_col(
        width = inferredBinWidth,
        fill = "steelblue",
        color = "white"
      ) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
      labs(
        title = paste0(
          componentName,
          " ",
          strMetricTitle,
          " Histogram Shape"
        ),
        subtitle = paste0(
          round(vectorExplainedVariance[componentIndex] * 100, 1),
          "% of histogram-shape variance; loading signs are arbitrary"
        ),
        x = strXAxisTitle,
        y = "PCA Loading"
      ) +
      theme_minimal()

    ggsave(
      filename = file.path(
        strFullOutputPath,
        paste0("histogramShape", componentName, "Loadings.png")
      ),
      plot = listLoadingPlots[[componentIndex]],
      width = intImageWidth,
      height = intImageHeight,
      units = "in",
      dpi = intImageDpi,
      bg = "white"
    )
  }

  plotLoadingStack <- wrap_plots(listLoadingPlots, ncol = 1)

  ggsave(
    filename = file.path(
      strFullOutputPath,
      "histogramShapePC1PC2PC3LoadingsStack.png"
    ),
    plot = plotLoadingStack,
    width = intImageWidth,
    height = intImageHeight * intLoadingComponents,
    units = "in",
    dpi = intImageDpi,
    bg = "white"
  )

  dfClosestPairs <- head(dfShapePairs, 25) %>%
    mutate(
      pairLabel = paste(
        firstGroup,
        secondGroup,
        sep = "\nmatched with\n"
      ),
      pairLabel = factor(pairLabel, levels = rev(pairLabel))
    )

  plotClosestPairs <- ggplot(
    dfClosestPairs,
    aes(x = hellingerSimilarity, y = pairLabel)
  ) +
    geom_col(fill = "steelblue") +
    scale_x_continuous(limits = c(0, 1)) +
    labs(
      title = paste0("Closest ", strMetricTitle, " Histogram Shapes"),
      subtitle = "Hellinger similarity: 1 indicates identical normalized shapes",
      x = "Hellinger Similarity",
      y = NULL
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 6))

  ggsave(
    filename = file.path(
      strFullOutputPath,
      "closestHistogramShapePairs.png"
    ),
    plot = plotClosestPairs,
    width = 14,
    height = 12,
    units = "in",
    dpi = intImageDpi,
    bg = "white"
  )

  cat(
    strMetricTitle,
    " histogram PCA complete. Groups compared: ",
    nrow(dfGroupMetadata),
    ". Outputs: ",
    strFullOutputPath,
    "\n",
    sep = ""
  )
}

############################################################
### Run All Five Histogram Analyses
############################################################

dir.create(strOutPath, recursive = TRUE, showWarnings = FALSE)

dfAnalysisPlan <- data.frame(
  statistic = c("max", "min", "median", "mean", "range"),
  outputFolder = c(
    "maximumTemperatureHistogram",
    "minimumTemperatureHistogram",
    "medianTemperatureHistogram",
    "meanTemperatureHistogram",
    "temperatureRangeHistogram"
  ),
  metricTitle = c(
    "Maximum Temperature",
    "Minimum Temperature",
    "Median Temperature",
    "Mean Temperature",
    "Temperature Range"
  ),
  xAxisTitle = c(
    rep("Temperature Bin Center (degrees C)", 4),
    "Temperature Range Bin Center (degrees C)"
  ),
  stringsAsFactors = FALSE
)

cat("PCA analysis plan:\n")
print(dfAnalysisPlan, row.names = FALSE)

for (analysisIndex in seq_len(nrow(dfAnalysisPlan))) {
  analyzeHistogramShapes(
    strHistogramPCAFilename,
    dfAnalysisPlan$statistic[analysisIndex],
    dfAnalysisPlan$outputFolder[analysisIndex],
    dfAnalysisPlan$metricTitle[analysisIndex],
    dfAnalysisPlan$xAxisTitle[analysisIndex]
  )

  gc()
}

cat("All five histogram-shape PCA analyses are complete.\n")
