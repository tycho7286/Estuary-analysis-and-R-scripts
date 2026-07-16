# PROJECT_CONTEXT.md

## Project identity

This project supports Kevin Witt's summer 2026 estuary sensor data work for Dr. John Largier at the UC Davis Bodega Marine Laboratory. The working project has also been called the **Estuary Project**, **BML UCDGAP**, and the **EMPA project**.

The immediate technical work is performed primarily in R. It combines, cleans, reconciles, analyzes, and plots large environmental sensor datasets from EMPA, California State Parks, and continuous Drakes Estero records. The broader scientific goal is to compare temperature, water depth, salinity, dissolved oxygen, tidal behavior, and estuary characteristics across many California estuaries.

This file is a handoff brief for Codex. Treat it as project context, not as proof that every older script or tentative scientific assumption is correct.

## User and collaboration context

- User: Kevin Witt.
- Supervisor: Dr. John Largier, commonly referred to as Dr. Largier.
- Summer work location: UC Davis Bodega Marine Laboratory.
- Kevin expects to leave the project in mid-August 2026.
- Daily summaries to Dr. Largier are often requested. These should be concise, casual but professional, and usually one short paragraph.
- Kevin is starting an Earth Science M.S. at Northern Illinois University in fall 2026.

## Primary goals

1. Build a reproducible cleaning and combination pipeline for heterogeneous estuary sensor data.
2. Preserve source values while adding standardized columns and calculated values.
3. Resolve sensor identity, deployment location, station, and date ambiguity.
4. Add accurate latitude and longitude metadata to observations or deployments.
5. Integrate NOAA tide data by finding the nearest station with usable data for each estuary and period.
6. Produce scalable plots and summary products for hundreds of estuaries.
7. Quantify or compile estuary characteristics, including distance from mouth and total estuary length.
8. Document data-quality problems and recommendations for EMPA maintainers.

## Current data sources

### EMPA

- Primary multi-estuary sensor dataset.
- Contains inconsistent sensor IDs, estuary assignments, units, coordinates, QA/QC fields, and known sensor error values.
- Some sensors appear assigned to the wrong estuary or to multiple estuaries over time.
- Deployment coordinates and metadata must be reconciled with dates and station assignments.

### California State Parks

- Source files have varying names and sensor column names.
- Cleaning currently detects columns by patterns such as `Water.Temperature`, `Water.Pressure`, `Diff.Pressure`, `Water.Level`, and `Barometric.Pressure`.
- Latitude and longitude supplied by Candice have been integrated in later work, although the attached older script does not include that newer integration.
- State Parks date fields have included two-digit years and required repeated correction.

### Continuous Drakes Estero data

- Structurally different from other sources and has required its own cleaning and combination scripts.
- Compatibility with the general workflow was still being developed.

### NOAA tide stations

- Intended for observed tide comparison and atmospheric or water-level context.
- A prior workflow downloaded NOAA data, but recent revisions caused many estuaries to lose NOAA coverage.
- The desired behavior is to search candidate stations by proximity and select the nearest station that actually has data for the required date range.
- This must scale to hundreds of estuaries. Do not hard-code a station for every estuary.
- Diagnostic outputs previously included station candidates, final station selection, and download diagnostics.

## Expected local data layout

The attached scripts use Windows paths under:

```text
C:/Users/Kevin/My Drive/School/2026Summer-BML-UCDGAP/
```

Important subdirectories seen in the scripts:

```text
Data/rawData/EMPA
Data/rawData/stateParks
Data/cleanData/EMPA
Data/cleanData/stateParks
Data/dataCombined
Data/dataWorking
```

Paths may differ on another computer or in a Git checkout. Prefer a small configuration section or project-relative paths in future refactoring. Do not silently replace Kevin's actual paths without confirming the execution environment.

## Current staged R workflow

### Stage 1: Source-specific cleaning

- EMPA files are read individually.
- `sensorid` is coerced to character to prevent loss of identity or binding conflicts.
- Unused analytes such as pH, turbidity, chlorophyll, and ORP are removed in the current EMPA cleaning script.
- Rows with blank or `unknown` sensor types are removed.
- Each cleaned source file is written separately.
- State Parks files are given `projectid = "State-Parks"`, an estuary name derived from the filename, a UTC `DateTime`, and standardized raw value and unit columns.

### Stage 2: Combine cleaned files

- Cleaned EMPA and State Parks CSV files are loaded into a named list.
- Column types are normalized before binding, especially `sensorid` and sensor-specific source columns.
- Standardized measurement columns are coerced to numeric with warnings suppressed.
- Files are combined with `dplyr::bind_rows()` and written as `combinedDataset.csv`.
- The combined dataset may approach roughly 10 million rows, so RDS should be preferred for repeated work when possible.

### Stage 3: Standardize units and calculate values

The current `3.0datasetInspectionAndValueCalculation.R` script:

- Converts `raw_depth` in meters or centimeters to `rawWaterDepthMeters`.
- Converts pressure from `cmH2O`, `mbar`, `psi`, `kPa`, or `dbar` to `rawPressureCm` in `cmH2O`.
- Calculates `calculatedWaterDepthMeters` from a fitted pressure relationship.
- Calculates salinity using `oce::swSCTp()` from conductivity, temperature, and pressure.
- Copies positive dissolved oxygen percent readings into `calculatedDOPct` and removes nonpositive values from consideration.
- Creates a UTC POSIXct `DateTime`, preferring `time_utc` over `time`.

Current constants in the attached script:

```r
depthVsPressureIntercept <- 3.837748327e-04
depthVsPressureSlope <- 9.881125021e-03
intConductivityConstant <- 42.914
intCmH2OToDbar <- 101.971621297793
```

These constants and the fitted depth equation need scientific validation and provenance before being treated as final methodology.

### Stage 4: Diagnostics and analysis

- Compare reported depth with calculated depth and raw pressure.
- Isolate low-depth, low-pressure, and high-pressure/low-depth regions.
- Investigate anomalous sensor groups and unit problems.
- Generate time-series plots for depth, temperature, salinity, and dissolved oxygen.
- Inspect duplicate timestamps, chronological order, sampling interval, and numeric types.
- Experimental FFT and periodicity analysis appears in `randomCodeSnipptes.R`.

### Stage 5: Plotting and summaries

Work completed or attempted includes:

- Temperature histograms.
- Five-number summary plots ordered south to north.
- Seasonal stacked plots for Jan to Mar, Apr to Jun, Jul to Sep, and Oct to Dec.
- Waterfall or vertically stacked comparisons.
- Depth time series, often paired with NOAA tide data.
- Per-year and per-quarter plot loops.

## Standardized fields and mapping decisions

- Preserve raw source values separately from calculated or converted values.
- Keep explicit unit columns.
- `Water Level` maps to the `raw_water_level` value and unit pair.
- `Diff. Pressure` currently maps to the `raw_depth` value and unit pair in the State Parks import.
- `raw_water_level_unit` should be `"Not Recorded"` when the value is absent and `"m"` when a value exists.
- Missing units should not be converted into misleading units.
- Use `"Not Recorded"` only where the workflow explicitly requires it.
- Source-specific raw columns may need to remain character until after files are reconciled, but standardized measurement columns used for math must be numeric.

## Important technical lessons

### Safe indexing with missing values

Logical indexes that compare a unit column can contain `NA`, which is invalid in subscripted assignments. Always guard unit comparisons:

```r
idxM <- !is.na(estuaryCombined$raw_depth_unit) &
  estuaryCombined$raw_depth_unit == "m"
```

For counts, use `na.rm = TRUE`.

### Numeric types

Some calculated columns have accidentally become character. Convert explicitly before summaries or plotting:

```r
df$calculatedDOPct <- as.numeric(df$calculatedDOPct)
```

### Time-series checks

Before time-series analysis:

1. Convert timestamps to POSIXct with an explicit timezone.
2. Sort by timestamp.
3. Check duplicate timestamps.
4. Check sampling intervals.
5. Verify measurement columns are numeric.

For one Drakes dissolved oxygen window, there were 2,880 rows, 2,880 unique timestamps, no duplicate times, and monotonically increasing timestamps. The immediate problem was that `calculatedDOPct` was character.

### Performance

- Avoid repeatedly reading and writing huge CSV files during analysis.
- RDS has already been used successfully for much faster loading.
- A Noyo River subset contained 175,193 rows.
- Plot loops may need to run overnight.
- Rasterization with `ggrastr` can help for dense plots.

## Known data-quality problems

### Sensor identity and location

- Sensor IDs and estuary assignments can conflict.
- The same sensor may appear in multiple estuaries or stations.
- Metadata coordinates do not always agree with the estuary label.
- Stations can move between deployments and seasons.
- Sensor location must therefore be represented as a deployment with coordinates and start and end dates, not as a timeless sensor attribute.

### Clustering coordinates

- DBSCAN has been used to examine unique station coordinates.
- A 25 m clustering radius was specifically tested.
- Review outputs excluded estuary/station groups that formed only one cluster or contained only one entry.
- KML files were created for Google Earth.
- Desired KML styling: different stations within an estuary should have visibly different pin colors, but clusters should not receive separate colors merely because they are separate clusters.

### Units and depth

- Reported water depth and depth calculated from pressure do not consistently follow the expected one-to-one relationship.
- Pressure units have been incorrect or inconsistent in some records.
- Large negative values can be sensor diagnostic/error codes rather than physical readings.
- The State Parks cleaning script currently labels `Diff. Pressure` stored in `raw_depth` as `kPa`. This field name/unit pairing is scientifically ambiguous and must be verified before treating it as physical depth.
- Atmospheric pressure is often absent, even where depth derivation may require atmospheric correction.

### QA/QC and error codes

- Known sensor error values can appear in the measurement stream as though they were observations.
- QA/QC flags exist, but their meanings and use have not always been visible or consistent.
- Raw values should be retained. Invalid or diagnostic values should receive explicit standardized flags rather than being silently destroyed.

### Dates

- State Parks source files included two-digit year formats.
- More date errors were found after an initial correction.
- Date parsing needs validation through plausible ranges, not only successful POSIXct conversion.

### NOAA coverage regression

- Earlier plotting code found NOAA data for more estuaries.
- Later code revisions reduced the number with NOAA data.
- Diagnose candidate selection, date-window availability checks, station ranking, request limits, and download parsing before changing plots.

## EMPA observations and recommendations report

A report was developed for EMPA maintainers in a non-adversarial, passive scientific voice. Version 1.0.1 was considered strong, followed by a selective editorial pass. It was intended to provide observations and recommendations from a data user, not to assign blame.

The central recommendations were:

1. Store deployment latitude and longitude directly with each deployment record, along with sensor ID, station, estuary, start and end dates, profile, water depth at deployment, and notes.
2. Standardize and document units for every raw and derived variable.
3. Document calculation methods and assumptions for derived variables such as water depth.
4. Detect known sensor error codes at ingestion and attach standardized QA/QC or diagnostic flags while preserving originals.
5. Publish clear definitions for all QA/QC flags and make them visible on the data request page.
6. Consider an EMPA-defined default QA/QC filter, with an option for users to request excluded values.
7. If atmospheric pressure is needed, populate it from a nearby station using deployment coordinates and timestamps, and store the source.
8. Rename `raw_depth` if it is actually a derived quantity.

The report is also being used to populate a presentation. Kevin wants to build the presentation himself and needs concise slide points, not an automatically generated deck.

## Current open work and likely priorities

1. Stabilize and document the master cleaning pipeline.
2. Incorporate the newest GPS metadata, including State Parks coordinates from Candice and pending or later coordinates from Nicolas.
3. Build a deployment-level sensor location/date table.
4. Decide how coordinate clusters should translate into stations or deployments.
5. Repair NOAA station discovery and data availability logic.
6. Finish support for continuous Drakes Estero data.
7. Measure distance from estuary mouth and total estuary length, probably using a GIS or Google Earth workflow with documented definitions.
8. Continue scalable seasonal, yearly, and quarter-based summaries.
9. Validate depth, salinity, oxygen, pressure, and unit calculations scientifically.
10. Keep report and presentation language concise and avoid unnecessary repetition.

## Attached source-file inventory

The source files used to prepare this brief were:

| File | Role | Status or caution |
| --- | --- | --- |
| `SSU-Chat-Summary-R-Code.txt` | Prior R-project handoff summary | Useful source of decisions and known errors |
| `1.0cleaningEMPAIndividualDatasetStep01.R` | EMPA per-file cleaning | Current-looking early-stage script, uses absolute Windows paths |
| `1.1cleaningStateParksIndividualDatasetStep01 - Copy.R` | State Parks per-file cleaning | Needs date and unit validation, name marked as Copy |
| `1.2cleaningDrakesIndividualDatasetStep01.R` | Drakes source cleaning | Older and very similar to EMPA script, verify paths and schema |
| `1.9combiningDrakesDatasets.R` | Drakes combination | Separate older combine workflow |
| `2.0combiningDatasets.R` | EMPA and State Parks combination | Most relevant attached combining script |
| `3.0datasetInspectionAndValueCalculation.R` | Unit normalization and calculations | Most relevant attached calculation script, output write is commented |
| `Old3.0datasetInspectionAndValueCalculation.R` | Older calculation and inspection code | Reference only, do not treat as current |
| `4.0plotWaterDepth.R` | Pressure/depth diagnostics and subsets | Exploratory, contains active writes and extensive old code |
| `4.1analysisOfOddDatapoints.R` | Anomaly analysis | Exploratory |
| `5.00timeSeriesDrakes.R` | Drakes time-series analysis | Exploratory and partly commented |
| `5.01timeSeriesRussian.R` | Russian River time-series analysis | Exploratory and partly commented |
| `randomCodeSnipptes.R` | FFT and miscellaneous experiments | Scratch/reference code, not a pipeline stage |

## Coding preferences and conventions

- Provide complete scripts when Kevin asks for a file, not only fragments.
- Use camelCase for function and method names.
- Use compact three-line headers for major code sections:

```r
############################################################
### Section Name
############################################################
```

- Put a one-line `###` comment immediately above functions or important reminders.
- Avoid R or Python docstrings unless explicitly requested.
- Avoid em dashes and en dashes in all writing and code comments.
- Prefer practical code that can be pasted and run.
- Use `vim`, not `nano`, in Linux instructions.
- Preserve raw data and make transformations explicit.
- Avoid destructive cleanup or overwriting source data.
- When revising an existing script, keep unrelated working behavior intact.

## Recommended Codex working approach

1. Inspect the actual repository and newest scripts before assuming the attached versions are current.
2. Identify an authoritative pipeline for each numbered stage and clearly label archival or experimental files.
3. Centralize paths and constants near the top of scripts or in a configuration file.
4. Add assertions or diagnostic tables for schema, unit values, date ranges, coordinate ranges, duplicates, and row counts at every stage.
5. Prefer RDS for intermediate large datasets, retaining CSV only where interoperability or delivery requires it.
6. Do not silently discard invalid data. Preserve the original field and add cleaned/calculated fields plus flags.
7. Verify scientific assumptions, especially pressure type, atmospheric correction, conductivity conversion, salinity formulas, and depth regressions.
8. Test on a small set of representative estuaries before launching all-estuary runs.
9. For NOAA integration, log every candidate station, distance, requested interval, availability result, error, and final selection.
10. Return generated scripts as downloadable files promptly when requested.

## Communication preferences

- Lead with the result.
- Keep explanations clear and practical.
- Do not overstate certainty when data fields or units are ambiguous.
- Kevin often prefers shorter conclusions and less repetitive prose.
- For emails, always let Kevin review the final draft before sending.
- Do not use em dashes or en dashes.

## Definition of a good next handoff

A future Codex session should be able to use this file plus the actual Git repository to determine:

- which script is authoritative for each pipeline stage,
- where the input data are expected,
- which fields are raw versus standardized or calculated,
- which assumptions remain unverified,
- what outputs are expected,
- and which scientific and technical tasks remain open.

If repository code conflicts with this brief, inspect dates and version history, then treat the newest verified working code as authoritative and update this file accordingly.
