############################################################
### Install and Load Packages
############################################################

# No additional packages are required.

############################################################
### File Paths
############################################################

# Windows
strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
strMetadataPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/metadata"

# # Linux
# strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
# strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataWorking"
# strMetadataPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/metadata"

strReadFilename <- "datasetWorkingCopy.rds"
strWriteFilename <- "datasetWorkingCopy.rds"
strPrefilteredBackupFilename <- "prefilteredWorkingDataset.rds"
strSummaryFilename <- "datasetQCFilterSummary.csv"
strStatisticsFilename <- "datasetQCFilterStatistics.csv"
strMissingQCReportFilename <- "datasetQCMissingFlagEstuaryReport.csv"
strFailedRowsFilename <- "qcFilterFailedRows.csv"

strFullReadName <- file.path(strInPath, strReadFilename)
strFullWriteName <- file.path(strOutPath, strWriteFilename)
strFullPrefilteredBackupName <- file.path(
  strOutPath,
  strPrefilteredBackupFilename
)
strFullSummaryName <- file.path(strMetadataPath, strSummaryFilename)
strFullStatisticsName <- file.path(strMetadataPath, strStatisticsFilename)
strFullMissingQCReportName <- file.path(strMetadataPath, strMissingQCReportFilename)
strFullFailedRowsName <- file.path(strMetadataPath, strFailedRowsFilename)

############################################################
### Filter Settings
############################################################

strQCFilterVersion <- "1.0"
strQCFilterCreatedAt <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

### Set the accepted values separately for every QC-flag column.
listAcceptedQCFlagsByColumn <- list(
  raw_depth_qcflag = c("-1", "0", "1", "2", "3", "4", "5", "6"),
  raw_pressure_qcflag = c("-1", "0", "1", "2", "3", "4", "5", "6"),
  raw_h2otemp_qcflag = c("-1", "0", "1", "2", "3", "4", "5", "6"),
  raw_ph_qcflag = c("-1", "0", "1", "2", "3", "4", "5", "6"),
  raw_conductivity_qcflag = c("-1", "0", "1", "2", "3", "4", "5", "6"),
  raw_turbidity_qcflag = c("-1", "0", "1", "2", "3", "4", "5", "6"),
  raw_do_qcflag = c("-1", "0", "1", "2", "3", "4", "5", "6"),
  raw_do_pct_qcflag = c("-1", "0", "1", "2", "3", "4", "5", "6"),
  raw_salinity_qcflag = c("-1", "0", "1", "2", "3", "4", "5", "6"),
  raw_chlorophyll_qcflag = c("-1", "0", "1", "2", "3", "4", "5", "6"),
  raw_orp_qcflag = c("-1", "0", "1", "2", "3", "4", "5", "6"),
  raw_qvalue_qcflag = c("-1", "0", "1", "2", "3", "4", "5", "6"),
  raw_atmospheric_pressure_qcflag = c("-1", "0", "1", "2", "3", "4", "5", "6")
)

### Missing, blank, NA, and NaN QC flags are reported but never filtered.

### Write failed original rows in chunks to limit memory use on the full dataset.
intFailedRowWriteChunkSize <- 100000L

############################################################
### Helper Functions
############################################################

### Convert QC flags to trimmed character values with missing codes represented as NA.
cleanQCFlag <- function(qcFlag) {
  qcFlag <- trimws(as.character(qcFlag))
  qcFlag[qcFlag %in% c("", "NA", "NaN")] <- NA_character_
  qcFlag
}

### Identify how a missing QC flag appeared in the source data.
classifyMissingQCFlag <- function(qcFlag) {
  qcFlagCharacter <- trimws(as.character(qcFlag))
  missingType <- rep(NA_character_, length(qcFlagCharacter))
  missingType[is.na(qcFlag)] <- "NA"
  missingType[!is.na(qcFlagCharacter) & qcFlagCharacter == ""] <- "Blank"
  missingType[!is.na(qcFlagCharacter) & qcFlagCharacter == "NA"] <- "NA text"
  missingType[!is.na(qcFlagCharacter) & qcFlagCharacter == "NaN"] <- "NaN"
  missingType
}

### Collapse observed QC flags into one diagnostic field.
collapseQCFlags <- function(qcFlag) {
  qcFlag <- sort(unique(qcFlag[!is.na(qcFlag)]))

  if (length(qcFlag) == 0) {
    return(NA_character_)
  }

  paste(qcFlag, collapse = " | ")
}

############################################################
### Validate Paths and Read Data
############################################################

if (!file.exists(strFullReadName)) {
  stop("Input dataset does not exist: ", strFullReadName)
}

dir.create(strOutPath, recursive = TRUE, showWarnings = FALSE)
dir.create(strMetadataPath, recursive = TRUE, showWarnings = FALSE)

############################################################
### Back Up Working Dataset Before Filtering
############################################################

backupCreated <- file.copy(
  from = strFullReadName,
  to = strFullPrefilteredBackupName,
  overwrite = TRUE
)

if (!backupCreated) {
  stop(
    "Could not create the pre-filter backup at: ",
    strFullPrefilteredBackupName
  )
}

cat(
  "Copied the unfiltered working dataset to: ",
  strFullPrefilteredBackupName,
  "\n",
  sep = ""
)

cat("Reading dataset from: ", strFullReadName, "\n", sep = "")
estuaryQCFiltered <- readRDS(strFullReadName)
cat("Rows read: ", format(nrow(estuaryQCFiltered), big.mark = ","), "\n", sep = "")

requiredFields <- c("estuaryname")
missingFields <- setdiff(requiredFields, names(estuaryQCFiltered))

if (length(missingFields) > 0) {
  stop("Input dataset is missing required fields: ", paste(missingFields, collapse = ", "))
}

qcFlagColumns <- grep("_qcflag$", names(estuaryQCFiltered), value = TRUE)

if (length(qcFlagColumns) == 0) {
  stop("No columns ending in _qcflag were found in the input dataset.")
}

configuredQCColumns <- names(listAcceptedQCFlagsByColumn)
unconfiguredQCColumns <- setdiff(qcFlagColumns, configuredQCColumns)

if (length(unconfiguredQCColumns) > 0) {
  stop(
    "Add accepted QC flags near the top of the script for: ",
    paste(unconfiguredQCColumns, collapse = ", ")
  )
}

############################################################
### Report Unexpected QC Flag Values
############################################################

unexpectedQCFlagCount <- 0L

for (qcFlagColumn in qcFlagColumns) {
  qcFlagClean <- cleanQCFlag(estuaryQCFiltered[[qcFlagColumn]])
  qcFlagNumeric <- suppressWarnings(as.numeric(qcFlagClean))
  idxUnexpectedQCFlag <- !is.na(qcFlagClean) &
    (
      is.na(qcFlagNumeric) |
        qcFlagNumeric < -5 |
        qcFlagNumeric > 6
    )

  if (any(idxUnexpectedQCFlag)) {
    unexpectedCounts <- sort(
      table(qcFlagClean[idxUnexpectedQCFlag]),
      decreasing = TRUE
    )
    unexpectedQCFlagCount <- unexpectedQCFlagCount +
      sum(unexpectedCounts)

    cat("Unexpected QC flags in ", qcFlagColumn, ":\n", sep = "")

    for (unexpectedValue in names(unexpectedCounts)) {
      cat(
        "  ",
        unexpectedValue,
        " (n = ",
        format(unexpectedCounts[[unexpectedValue]], big.mark = ","),
        ")\n",
        sep = ""
      )
    }
  }

  rm(qcFlagClean, qcFlagNumeric, idxUnexpectedQCFlag)

  if (exists("unexpectedCounts")) {
    rm(unexpectedCounts)
  }

  gc()
}

if (unexpectedQCFlagCount == 0) {
  cat("No nonmissing QC flags outside the expected range of -5 through 6 were found.\n")
} else {
  cat(
    "Total unexpected QC flag entries: ",
    format(unexpectedQCFlagCount, big.mark = ","),
    "\n",
    sep = ""
  )
}

############################################################
### Write Original Rows That Fail One or More QC Filters
############################################################

idxAnyQCFailure <- rep(FALSE, nrow(estuaryQCFiltered))

for (qcFlagColumn in qcFlagColumns) {
  measurementColumn <- sub("_qcflag$", "", qcFlagColumn)

  if (!measurementColumn %in% names(estuaryQCFiltered)) {
    next
  }

  acceptedQCFlags <- as.character(listAcceptedQCFlagsByColumn[[qcFlagColumn]])
  qcFlagClean <- cleanQCFlag(estuaryQCFiltered[[qcFlagColumn]])
  idxFieldFailure <-
    !is.na(estuaryQCFiltered[[measurementColumn]]) &
    !is.na(qcFlagClean) &
    !qcFlagClean %in% acceptedQCFlags

  idxAnyQCFailure <- idxAnyQCFailure | idxFieldFailure

  rm(acceptedQCFlags, qcFlagClean, idxFieldFailure)
  gc()
}

failedRowIndexes <- which(idxAnyQCFailure)

if (length(failedRowIndexes) == 0) {
  write.csv(
    estuaryQCFiltered[FALSE, , drop = FALSE],
    strFullFailedRowsName,
    row.names = FALSE
  )
} else {
  if (file.exists(strFullFailedRowsName)) {
    file.remove(strFullFailedRowsName)
  }

  failedRowChunks <- split(
    failedRowIndexes,
    ceiling(seq_along(failedRowIndexes) / intFailedRowWriteChunkSize)
  )

  for (chunkIndex in seq_along(failedRowChunks)) {
    dfFailedRowChunk <- estuaryQCFiltered[
      failedRowChunks[[chunkIndex]],
      ,
      drop = FALSE
    ]

    write.table(
      dfFailedRowChunk,
      file = strFullFailedRowsName,
      sep = ",",
      row.names = FALSE,
      col.names = chunkIndex == 1,
      append = chunkIndex > 1,
      quote = TRUE,
      na = ""
    )

    rm(dfFailedRowChunk)
    gc()
  }

  rm(failedRowChunks)
}

cat(
  "Wrote ",
  format(length(failedRowIndexes), big.mark = ","),
  " unique original failed rows to: ",
  strFullFailedRowsName,
  "\n",
  sep = ""
)

############################################################
### Filter Measurements by Their Matching QC Flags
############################################################

qcSummaryList <- list()
missingQCReportList <- list()
summaryIndex <- 0L

for (qcFlagColumn in qcFlagColumns) {
  measurementColumn <- sub("_qcflag$", "", qcFlagColumn)

  if (!measurementColumn %in% names(estuaryQCFiltered)) {
    cat(
      "Skipping ",
      qcFlagColumn,
      ": matching measurement column ",
      measurementColumn,
      " was not found.\n",
      sep = ""
    )
    next
  }

  acceptedQCFlags <- as.character(listAcceptedQCFlagsByColumn[[qcFlagColumn]])
  qcFlagOriginal <- estuaryQCFiltered[[qcFlagColumn]]
  qcFlagClean <- cleanQCFlag(qcFlagOriginal)
  missingQCType <- classifyMissingQCFlag(qcFlagOriginal)
  idxHasMeasurement <- !is.na(estuaryQCFiltered[[measurementColumn]])
  idxMissingQCFlag <- is.na(qcFlagClean)
  idxMissingQCFlagWithMeasurement <- idxHasMeasurement & idxMissingQCFlag
  idxAcceptedQCFlag <- idxHasMeasurement &
    !is.na(qcFlagClean) &
    qcFlagClean %in% acceptedQCFlags
  idxDisallowedQCFlag <- idxHasMeasurement &
    !is.na(qcFlagClean) &
    !qcFlagClean %in% acceptedQCFlags
  idxRejected <- idxDisallowedQCFlag

  valuesSetToNA <- sum(idxRejected, na.rm = TRUE)

  if (valuesSetToNA > 0) {
    estuaryQCFiltered[[measurementColumn]][idxRejected] <- NA
  }

  summaryIndex <- summaryIndex + 1L
  qcSummaryList[[summaryIndex]] <- data.frame(
    qcFilterVersion = strQCFilterVersion,
    qcFilterCreatedAt = strQCFilterCreatedAt,
    measurementColumn = measurementColumn,
    qcFlagColumn = qcFlagColumn,
    acceptedQCFlags = paste(acceptedQCFlags, collapse = " | "),
    measurementsEvaluated = sum(idxHasMeasurement, na.rm = TRUE),
    acceptedMeasurements = sum(idxAcceptedQCFlag, na.rm = TRUE),
    missingQCFlags = sum(idxMissingQCFlag, na.rm = TRUE),
    measurementsWithMissingQCFlags = sum(
      idxMissingQCFlagWithMeasurement,
      na.rm = TRUE
    ),
    disallowedQCFlags = sum(idxDisallowedQCFlag, na.rm = TRUE),
    valuesSetToNA = valuesSetToNA,
    observedQCFlags = collapseQCFlags(qcFlagClean),
    stringsAsFactors = FALSE
  )

  if (any(idxMissingQCFlag)) {
    estuaryClean <- trimws(as.character(estuaryQCFiltered$estuaryname))
    estuaryClean[is.na(estuaryClean) | estuaryClean == ""] <- "Not Recorded"

    dfMissingQC <- data.frame(
      estuaryname = estuaryClean[idxMissingQCFlag],
      missingQCType = missingQCType[idxMissingQCFlag],
      rows = 1L,
      rowsWithMeasurement = as.integer(idxHasMeasurement[idxMissingQCFlag]),
      stringsAsFactors = FALSE
    )

    dfMissingQCAggregated <- aggregate(
      cbind(rows, rowsWithMeasurement) ~ estuaryname + missingQCType,
      data = dfMissingQC,
      FUN = sum
    )
    dfMissingQCAggregated$measurementColumn <- measurementColumn
    dfMissingQCAggregated$qcFlagColumn <- qcFlagColumn
    dfMissingQCAggregated <- dfMissingQCAggregated[, c(
      "measurementColumn",
      "qcFlagColumn",
      "estuaryname",
      "missingQCType",
      "rows",
      "rowsWithMeasurement"
    )]
    missingQCReportList[[qcFlagColumn]] <- dfMissingQCAggregated
  }

  cat(
    measurementColumn,
    ": ",
    format(valuesSetToNA, big.mark = ","),
    " values set to NA.\n",
    sep = ""
  )

  rm(
    acceptedQCFlags,
    qcFlagOriginal,
    qcFlagClean,
    missingQCType,
    idxHasMeasurement,
    idxMissingQCFlag,
    idxMissingQCFlagWithMeasurement,
    idxAcceptedQCFlag,
    idxDisallowedQCFlag,
    idxRejected
  )
  gc()
}

############################################################
### Create and Write Diagnostics
############################################################

if (length(qcSummaryList) == 0) {
  stop("No matching measurement and QC-flag column pairs were found.")
}

dfQCFilterSummary <- do.call(rbind, qcSummaryList)

if (length(missingQCReportList) > 0) {
  dfMissingQCReport <- do.call(rbind, missingQCReportList)
  row.names(dfMissingQCReport) <- NULL
} else {
  dfMissingQCReport <- data.frame(
    measurementColumn = character(),
    qcFlagColumn = character(),
    estuaryname = character(),
    missingQCType = character(),
    rows = integer(),
    rowsWithMeasurement = integer(),
    stringsAsFactors = FALSE
  )
}

dfQCFilterStatistics <- data.frame(
  qcFilterVersion = strQCFilterVersion,
  qcFilterCreatedAt = strQCFilterCreatedAt,
  inputFile = strReadFilename,
  outputFile = strWriteFilename,
  rowsRead = nrow(estuaryQCFiltered),
  rowsWithOneOrMoreQCFailures = length(failedRowIndexes),
  measurementFieldsFiltered = nrow(dfQCFilterSummary),
  totalMeasurementsEvaluated = sum(dfQCFilterSummary$measurementsEvaluated),
  totalAcceptedMeasurements = sum(dfQCFilterSummary$acceptedMeasurements),
  totalMissingQCFlags = sum(dfQCFilterSummary$missingQCFlags),
  totalMeasurementsWithMissingQCFlags = sum(
    dfQCFilterSummary$measurementsWithMissingQCFlags
  ),
  totalDisallowedQCFlags = sum(dfQCFilterSummary$disallowedQCFlags),
  totalValuesSetToNA = sum(dfQCFilterSummary$valuesSetToNA),
  stringsAsFactors = FALSE
)

write.csv(dfQCFilterSummary, strFullSummaryName, row.names = FALSE)
write.csv(dfQCFilterStatistics, strFullStatisticsName, row.names = FALSE)
write.csv(dfMissingQCReport, strFullMissingQCReportName, row.names = FALSE)

############################################################
### Write Filtered Dataset
############################################################

cat("Saving QC-filtered dataset to: ", strFullWriteName, "\n", sep = "")
saveRDS(estuaryQCFiltered, strFullWriteName)

cat("Wrote QC summary to: ", strFullSummaryName, "\n", sep = "")
cat("Wrote QC statistics to: ", strFullStatisticsName, "\n", sep = "")
cat("Wrote missing-QC estuary report to: ", strFullMissingQCReportName, "\n", sep = "")
cat("Wrote original failed rows to: ", strFullFailedRowsName, "\n", sep = "")
cat(
  "Total values set to NA: ",
  format(dfQCFilterStatistics$totalValuesSetToNA, big.mark = ","),
  "\n",
  sep = ""
)

############################################################
### Garbage Collector
############################################################

gc()
