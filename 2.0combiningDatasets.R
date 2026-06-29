# install.packages("dplyr")
library(dplyr)

#Windows
strInPaths <- c(
  "D:/Google/School/2026Summer-BML-UCDGAP/Data/cleanData/EMPA",
  "D:/Google/School/2026Summer-BML-UCDGAP/Data/cleanData/stateParks"
)
strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataCombined"

# #Linux
# strInPaths <- c(
#   "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/cleanData/EMPA",
#   "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/cleanData/stateParks"
# )
# strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataCombined"


strWriteFilename <- "combinedDataset.rds"

dir.create(strOutPath, recursive = TRUE, showWarnings = FALSE)

fileList <- unlist(lapply(strInPaths, function(path) {
  file.path(path, list.files(path, pattern = "\\.rds$", full.names = FALSE))
}))

estuaryList <- list()
estuaryCombined <- data.frame()

for (index in seq_along(fileList)) {
  
  strFullName <- fileList[index]
  fileName <- basename(strFullName)
  
  dfImport <- readRDS(strFullName)
  
  if ("sensorid" %in% names(dfImport)) {
    dfImport$sensorid <- as.character(dfImport$sensorid)
  }
  
  sensorCols <- grep(
    "Water\\.Temperature|Diff\\.Pressure|Water\\.Pressure|Barometric\\.Pressure|Battery|Water\\.Level",
    names(dfImport),
    value = TRUE
  )
  
  for (col in sensorCols) {
    dfImport[[col]] <- as.character(dfImport[[col]])
  }
  
  numericCols <- c(
    "raw_depth",
    "raw_water_level",
    "raw_pressure",
    "raw_h2otemp",
    "raw_atmospheric_pressure",
    "raw_conductivity",
    "raw_do",
    "raw_do_pct",
    "raw_salinity",
    "raw_qvalue"
  )
  
  for (col in numericCols) {
    if (col %in% names(dfImport)) {
      dfImport[[col]] <- suppressWarnings(as.numeric(dfImport[[col]]))
    }
  }
  
  estuaryList[[fileName]] <- dfImport
  
  cat(index, fileName, "\n")
}

estuaryCombined <- bind_rows(estuaryList)

fullWriteName <- file.path(strOutPath, strWriteFilename)

saveRDS(estuaryCombined, fullWriteName)

print(object.size(estuaryList) / 1024^2)
print(object.size(estuaryCombined) / 1024^2)

gc()