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
strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/plotsAndImages/rStudioPlotOutputs/pcaHistogramShapeAnalysis"

# # Linux
# strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/plotsAndImages/rStudioPlotOutputs"
# strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/plotsAndImages/rStudioPlotOutputs/pcaHistogramShapeAnalysis"

strReadFilename <- "temperatureWindowPCAData.csv"

### Choose the window-summary variable whose histogram shape will be compared.
strHistogramValueColumn <- "meanTemp"

### All groups use these same absolute bins and bin origin.
dblHistogramBinWidth <- 0.5
dblHistogramBinBoundary <- 0

### Groups must contain at least this many window rows.
intMinimumWindows <- 51

### Number of closest histogram-shape matches reported for every group.
intNearestMatches <- 5

intImageWidth <- 10
intImageHeight <- 7
intImageDpi <- 600

strFullReadName <- file.path(strInPath, strReadFilename)

############################################################
### Helper Functions
############################################################

### Make a unique readable label for one histogram group.
makeGroupLabel <- function(region, estuary, station, profile, season) {
  paste(region, estuary, paste0("Station ", station), profile, season, sep = " | ")
}

### Make safe column labels for absolute histogram bins.
makeBinName <- function(binLower, binUpper) {
  cleanNumber <- function(x) {
    x <- format(x, trim = TRUE, scientific = FALSE)
    x <- sub("^-", "neg", x)
    gsub("\\.", "p", x)
  }

  paste0("bin", cleanNumber(binLower), "To", cleanNumber(binUpper))
}

### Calculate a normalized histogram using shared absolute breaks.
makeHistogramProportions <- function(x, histogramBreaks) {
  x <- x[is.finite(x)]
  histogramCounts <- hist(
    x,
    breaks = histogramBreaks,
    plot = FALSE,
    right = FALSE,
    include.lowest = TRUE
  )$counts

  if (sum(histogramCounts) == 0) {
    return(rep(0, length(histogramCounts)))
  }

  histogramCounts / sum(histogramCounts)
}

############################################################
### Read and Validate PCA Window Data
############################################################

if (!file.exists(strFullReadName)) {
  stop("Input PCA data file does not exist: ", strFullReadName)
}

dir.create(strOutPath, recursive = TRUE, showWarnings = FALSE)

dfPCAInput <- read.csv(strFullReadName, stringsAsFactors = FALSE)

groupFields <- c("region", "estuary", "station", "profile", "season")
requiredFields <- c(groupFields, strHistogramValueColumn)
missingFields <- setdiff(requiredFields, names(dfPCAInput))

if (length(missingFields) > 0) {
  stop("Input PCA data is missing required fields: ", paste(missingFields, collapse = ", "))
}

dfPCAInput[[strHistogramValueColumn]] <- suppressWarnings(
  as.numeric(dfPCAInput[[strHistogramValueColumn]])
)

for (groupField in groupFields) {
  dfPCAInput[[groupField]] <- trimws(as.character(dfPCAInput[[groupField]]))
  dfPCAInput[[groupField]][
    is.na(dfPCAInput[[groupField]]) | dfPCAInput[[groupField]] == ""
  ] <- "Not Recorded"
}

dfPCAInput <- dfPCAInput %>%
  filter(is.finite(.data[[strHistogramValueColumn]])) %>%
  mutate(
    groupKey = paste(region, estuary, station, profile, season, sep = "\r"),
    groupLabel = makeGroupLabel(region, estuary, station, profile, season)
  )

if (nrow(dfPCAInput) == 0) {
  stop("No finite values were found in: ", strHistogramValueColumn)
}

############################################################
### Identify Qualifying Histogram Groups
############################################################

dfGroupMetadata <- dfPCAInput %>%
  group_by(groupKey, region, estuary, station, profile, season, groupLabel) %>%
  summarise(windowCount = n(), .groups = "drop") %>%
  filter(windowCount >= intMinimumWindows) %>%
  arrange(region, estuary, station, profile, season)

if (nrow(dfGroupMetadata) < 2) {
  stop("At least two qualifying histogram groups are required for similarity analysis.")
}

cat(
  "Histogram groups meeting the minimum of ",
  intMinimumWindows,
  " windows: ",
  nrow(dfGroupMetadata),
  "\n",
  sep = ""
)

############################################################
### Create Common Absolute Histogram Bins
############################################################

minimumValue <- min(dfPCAInput[[strHistogramValueColumn]], na.rm = TRUE)
maximumValue <- max(dfPCAInput[[strHistogramValueColumn]], na.rm = TRUE)

minimumBreak <- floor(
  (minimumValue - dblHistogramBinBoundary) / dblHistogramBinWidth
) * dblHistogramBinWidth + dblHistogramBinBoundary

maximumBreak <- ceiling(
  (maximumValue - dblHistogramBinBoundary) / dblHistogramBinWidth
) * dblHistogramBinWidth + dblHistogramBinBoundary

if (maximumBreak <= maximumValue) {
  maximumBreak <- maximumBreak + dblHistogramBinWidth
}

histogramBreaks <- seq(
  minimumBreak,
  maximumBreak,
  by = dblHistogramBinWidth
)

binLower <- histogramBreaks[-length(histogramBreaks)]
binUpper <- histogramBreaks[-1]
binNames <- makeBinName(binLower, binUpper)

cat(
  "Using ",
  length(binNames),
  " shared bins from ",
  minimumBreak,
  " to ",
  maximumBreak,
  " with bin width ",
  dblHistogramBinWidth,
  ".\n",
  sep = ""
)

############################################################
### Build Histogram Shape Matrix
############################################################

histogramMatrix <- matrix(
  0,
  nrow = nrow(dfGroupMetadata),
  ncol = length(binNames),
  dimnames = list(dfGroupMetadata$groupLabel, binNames)
)

for (groupIndex in seq_len(nrow(dfGroupMetadata))) {
  currentGroupKey <- dfGroupMetadata$groupKey[groupIndex]
  currentValues <- dfPCAInput[[strHistogramValueColumn]][
    dfPCAInput$groupKey == currentGroupKey
  ]

  histogramMatrix[groupIndex, ] <- makeHistogramProportions(
    currentValues,
    histogramBreaks
  )
}

if (any(abs(rowSums(histogramMatrix) - 1) > 1e-10)) {
  stop("At least one normalized histogram does not sum to 1.")
}

############################################################
### Export Histogram Bin Proportions
############################################################

dfHistogramWide <- cbind(
  as.data.frame(dfGroupMetadata),
  as.data.frame(histogramMatrix, check.names = FALSE)
)

write.csv(
  dfHistogramWide,
  file.path(strOutPath, "histogramShapeBinProportionsWide.csv"),
  row.names = FALSE
)

dfHistogramLong <- data.frame(
  groupIndex = rep(seq_len(nrow(dfGroupMetadata)), each = length(binNames)),
  binLower = rep(binLower, times = nrow(dfGroupMetadata)),
  binUpper = rep(binUpper, times = nrow(dfGroupMetadata)),
  proportion = as.vector(t(histogramMatrix)),
  stringsAsFactors = FALSE
) %>%
  left_join(
    dfGroupMetadata %>%
      mutate(groupIndex = row_number()),
    by = "groupIndex"
  ) %>%
  select(
    region,
    estuary,
    station,
    profile,
    season,
    groupLabel,
    windowCount,
    binLower,
    binUpper,
    proportion
  )

write.csv(
  dfHistogramLong,
  file.path(strOutPath, "histogramShapeBinProportionsLong.csv"),
  row.names = FALSE
)

############################################################
### Calculate Direct Histogram Shape Similarity
############################################################

### Hellinger distance ranges from 0 for identical shapes to 1 for dissimilar shapes.
hellingerDistanceMatrix <- as.matrix(dist(sqrt(histogramMatrix))) / sqrt(2)
hellingerSimilarityMatrix <- 1 - hellingerDistanceMatrix

pairIndexes <- which(upper.tri(hellingerDistanceMatrix), arr.ind = TRUE)

dfShapePairs <- data.frame(
  firstGroupIndex = pairIndexes[, 1],
  secondGroupIndex = pairIndexes[, 2],
  hellingerDistance = hellingerDistanceMatrix[pairIndexes],
  hellingerSimilarity = hellingerSimilarityMatrix[pairIndexes],
  stringsAsFactors = FALSE
) %>%
  mutate(
    firstGroup = dfGroupMetadata$groupLabel[firstGroupIndex],
    secondGroup = dfGroupMetadata$groupLabel[secondGroupIndex]
  ) %>%
  select(
    firstGroup,
    secondGroup,
    hellingerDistance,
    hellingerSimilarity
  ) %>%
  arrange(hellingerDistance)

write.csv(
  dfShapePairs,
  file.path(strOutPath, "histogramShapeAllPairSimilarities.csv"),
  row.names = FALSE
)

nearestMatchList <- vector("list", nrow(dfGroupMetadata))

for (groupIndex in seq_len(nrow(dfGroupMetadata))) {
  orderedMatches <- order(hellingerDistanceMatrix[groupIndex, ])
  orderedMatches <- orderedMatches[orderedMatches != groupIndex]
  orderedMatches <- head(orderedMatches, intNearestMatches)

  nearestMatchList[[groupIndex]] <- data.frame(
    group = dfGroupMetadata$groupLabel[groupIndex],
    matchRank = seq_along(orderedMatches),
    matchingGroup = dfGroupMetadata$groupLabel[orderedMatches],
    hellingerDistance = hellingerDistanceMatrix[groupIndex, orderedMatches],
    hellingerSimilarity = hellingerSimilarityMatrix[groupIndex, orderedMatches],
    stringsAsFactors = FALSE
  )
}

dfNearestMatches <- bind_rows(nearestMatchList)

write.csv(
  dfNearestMatches,
  file.path(strOutPath, "histogramShapeNearestMatches.csv"),
  row.names = FALSE
)

############################################################
### Run PCA on Histogram Bin Proportions
############################################################

variableBins <- apply(histogramMatrix, 2, sd) > 0

if (sum(variableBins) < 2) {
  stop("At least two histogram bins with variation are required for PCA.")
}

pcaResult <- prcomp(
  histogramMatrix[, variableBins, drop = FALSE],
  center = TRUE,
  scale. = FALSE
)

explainedVariance <- pcaResult$sdev^2 / sum(pcaResult$sdev^2)

dfPCAVariance <- data.frame(
  component = paste0("PC", seq_along(explainedVariance)),
  explainedVariance = explainedVariance,
  cumulativeVariance = cumsum(explainedVariance),
  stringsAsFactors = FALSE
)

dfPCAScores <- cbind(
  as.data.frame(dfGroupMetadata),
  as.data.frame(pcaResult$x)
)

write.csv(
  dfPCAScores,
  file.path(strOutPath, "histogramShapePCAScores.csv"),
  row.names = FALSE
)

write.csv(
  dfPCAVariance,
  file.path(strOutPath, "histogramShapePCAVariance.csv"),
  row.names = FALSE
)

############################################################
### Plot PC1, PC2, and PC3 Histogram Shapes
############################################################

intLoadingComponents <- min(3, ncol(pcaResult$rotation))

dfPCALoadings <- data.frame(
  binLower = rep(binLower[variableBins], times = intLoadingComponents),
  binUpper = rep(binUpper[variableBins], times = intLoadingComponents),
  binCenter = rep(
    (binLower[variableBins] + binUpper[variableBins]) / 2,
    times = intLoadingComponents
  ),
  component = rep(
    paste0("PC", seq_len(intLoadingComponents)),
    each = sum(variableBins)
  ),
  loading = as.vector(
    pcaResult$rotation[
      ,
      seq_len(intLoadingComponents),
      drop = FALSE
    ]
  ),
  stringsAsFactors = FALSE
)

write.csv(
  dfPCALoadings,
  file.path(strOutPath, "histogramShapePCALoadings.csv"),
  row.names = FALSE
)

listPCALoadingPlots <- vector("list", intLoadingComponents)

for (componentIndex in seq_len(intLoadingComponents)) {
  componentName <- paste0("PC", componentIndex)
  dfCurrentLoadings <- dfPCALoadings %>%
    filter(component == componentName)

  listPCALoadingPlots[[componentIndex]] <- ggplot(
    dfCurrentLoadings,
    aes(x = binCenter, y = loading)
  ) +
    geom_col(
      width = dblHistogramBinWidth,
      fill = "steelblue",
      color = "white"
    ) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    labs(
      title = paste0(
        componentName,
        " ",
        strHistogramValueColumn,
        " Histogram Shape"
      ),
      subtitle = paste0(
        round(explainedVariance[componentIndex] * 100, 1),
        "% of histogram-shape variance; loading signs are arbitrary"
      ),
      x = paste0(
        strHistogramValueColumn,
        " Bin Center (degrees C)"
      ),
      y = "PCA Loading"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(
      strOutPath,
      paste0("histogramShape", componentName, "Loadings.png")
    ),
    plot = listPCALoadingPlots[[componentIndex]],
    width = intImageWidth,
    height = intImageHeight,
    units = "in",
    dpi = intImageDpi,
    bg = "white"
  )
}

plotPCALoadingStack <- wrap_plots(
  listPCALoadingPlots,
  ncol = 1
)

ggsave(
  filename = file.path(
    strOutPath,
    "histogramShapePC1PC2PC3LoadingsStack.png"
  ),
  plot = plotPCALoadingStack,
  width = intImageWidth,
  height = intImageHeight * intLoadingComponents,
  units = "in",
  dpi = intImageDpi,
  bg = "white"
)

if (intLoadingComponents < 3) {
  warning(
    "Only ",
    intLoadingComponents,
    " PCA loading components were available to plot."
  )
}

############################################################
### Create PCA and Similarity Plots
############################################################

if (all(c("PC1", "PC2") %in% names(dfPCAScores))) {
  plotPCA <- ggplot(
    dfPCAScores,
    aes(x = PC1, y = PC2, color = region, shape = profile)
  ) +
    geom_point(size = 2.2, alpha = 0.8) +
    labs(
      title = paste0(strHistogramValueColumn, " Histogram Shape PCA"),
      subtitle = paste0(
        "Shared ",
        dblHistogramBinWidth,
        " degree C bins; PCA uses normalized bin proportions"
      ),
      x = paste0("PC1 (", round(explainedVariance[1] * 100, 1), "%)"),
      y = paste0("PC2 (", round(explainedVariance[2] * 100, 1), "%)"),
      color = "Region",
      shape = "Profile"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(strOutPath, "histogramShapePCA.png"),
    plot = plotPCA,
    width = intImageWidth,
    height = intImageHeight,
    units = "in",
    dpi = intImageDpi,
    bg = "white"
  )
}

dfClosestPairs <- head(dfShapePairs, 25) %>%
  mutate(
    pairLabel = paste(firstGroup, secondGroup, sep = "\nmatched with\n"),
    pairLabel = factor(pairLabel, levels = rev(pairLabel))
  )

plotClosestPairs <- ggplot(
  dfClosestPairs,
  aes(x = hellingerSimilarity, y = pairLabel)
) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(limits = c(0, 1)) +
  labs(
    title = paste0("Closest ", strHistogramValueColumn, " Histogram Shapes"),
    subtitle = "Hellinger similarity: 1 indicates identical normalized histogram shapes",
    x = "Hellinger Similarity",
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

ggsave(
  filename = file.path(strOutPath, "closestHistogramShapePairs.png"),
  plot = plotClosestPairs,
  width = 14,
  height = 12,
  units = "in",
  dpi = intImageDpi,
  bg = "white"
)

############################################################
### Print Summary
############################################################

cat("Histogram-shape analysis complete.\n")
cat("Groups compared: ", nrow(dfGroupMetadata), "\n", sep = "")
cat(
  "PC1 and PC2 explained variance: ",
  round(sum(explainedVariance[1:min(2, length(explainedVariance))]) * 100, 1),
  "%\n",
  sep = ""
)
cat("Closest histogram-shape pairs:\n")
print(head(dfShapePairs, 10), row.names = FALSE)
cat("Outputs written to: ", strOutPath, "\n", sep = "")

gc()
