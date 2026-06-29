
# 
# 
# # Depth data
# dfFFTDepth <- dfDrakesDepth[, c("DateTime", "calculatedWaterDepthMeters")]
# dfFFTDepth <- dfFFTDepth[
#   !is.na(dfFFTDepth$DateTime) &
#     !is.na(dfFFTDepth$calculatedWaterDepthMeters),
# ]
# dfFFTDepth <- dfFFTDepth[order(dfFFTDepth$DateTime), ]
# 
# # Average duplicate timestamps
# dfFFTDepth <- aggregate(
#   calculatedWaterDepthMeters ~ DateTime,
#   data = dfFFTDepth,
#   FUN = mean
# )
# 
# # Check time spacing
# table(diff(dfFFTDepth$DateTime))
# 
# yDepth <- dfFFTDepth$calculatedWaterDepthMeters
# yDepth_detrended <- yDepth - mean(yDepth, na.rm = TRUE)
# 
# dt_hoursDepth <- as.numeric(
#   median(diff(dfFFTDepth$DateTime)),
#   units = "hours"
# )
# 
# fft_resultDepth <- fft(yDepth_detrended)
# 
# nDepth <- length(yDepth_detrended)
# freqDepth <- (0:(nDepth - 1)) / (nDepth * dt_hoursDepth)
# powerDepth <- Mod(fft_resultDepth)^2
# 
# dfFFTDepthResult <- data.frame(
#   frequency_cph = freqDepth[1:floor(nDepth / 2)],
#   period_hours = 1 / freqDepth[1:floor(nDepth / 2)],
#   power = powerDepth[1:floor(nDepth / 2)]
# )
# 
# dfFFTDepthResult <- dfFFTDepthResult[
#   is.finite(dfFFTDepthResult$period_hours),
# ]
# 
# 
# ggplot(dfFFTDepthResult, aes(x = period_hours, y = power)) +
#   geom_line() +
#   scale_x_log10() +
#   labs(
#     title = "FFT of Drakes Estero Water Depth",
#     x = "Period (hours)",
#     y = "Power"
#   ) +
#   theme_minimal()
# 
# 
# 
# 
# 
# plot1<-ggplot(dfFFTDepthResult[dfFFTDepthResult$period_hours < 40, ],
#        aes(x = period_hours, y = power)) +
#   geom_line() +
#   geom_vline(xintercept = 12.42, color = "red", linetype = "dashed") +
#   geom_vline(xintercept = 24.84, color = "blue", linetype = "dashed") +
#   labs(
#     title = "FFT of Drakes Estero Water Depth",
#     x = "Period (hours)",
#     y = "Power"
#   ) +
#   theme_minimal()
# plot1
# 
# 
# 
# # Use temperature data
# dfFFTTemp <- dfDrakesTemp[, c("DateTime", "raw_h2otemp")]
# dfFFTTemp <- dfFFTTemp[!is.na(dfFFTTemp$DateTime) & !is.na(dfFFTTemp$raw_h2otemp), ]
# dfFFTTemp <- dfFFTTemp[order(dfFFTTemp$DateTime), ]
# 
# # If duplicate timestamps exist, average them
# dfFFTTemp <- aggregate(raw_h2otemp ~ DateTime, data = dfFFTTemp, FUN = mean)
# 
# # Check time spacing
# table(diff(dfFFTTemp$DateTime))
# 
# # Temperature vector
# yTemp <- dfFFTTemp$raw_h2otemp
# 
# # Remove mean so FFT focuses on oscillations
# yTemp_detrended <- yTemp - mean(yTemp, na.rm = TRUE)
# 
# # Time step in hours
# dt_hoursTemp <- as.numeric(median(diff(dfFFTTemp$DateTime)), units = "hours")
# 
# # FFT
# fft_resultTemp <- fft(yTemp_detrended)
# 
# n <- length(yTemp_detrended)
# 
# # Frequencies in cycles per hour
# freq <- (0:(n - 1)) / (n * dt_hoursTemp)
# 
# # Power
# power <- Mod(fft_resultTemp)^2
# 
# # Keep only positive frequencies
# dfFFTTempResult <- data.frame(
#   frequency_cph = freq[1:floor(n / 2)],
#   period_hours = 1 / freq[1:floor(n / 2)],
#   power = power[1:floor(n / 2)]
# )
# 
# # Remove infinite period from frequency 0
# dfFFTTempResult <- dfFFTTempResult[
#   is.finite(dfFFTTempResult$period_hours),
# ]
# 
# ggplot(dfFFTTempResult, aes(x = period_hours, y = power)) +
#   geom_line() +
#   scale_x_log10() +
#   labs(
#     title = "FFT of Drakes Estero Water Temperature",
#     x = "Period (hours)",
#     y = "Power"
#   ) +
#   theme_minimal()
# 
# plot2 <- ggplot(
#   dfFFTTempResult[dfFFTTempResult$period_hours < 40, ],
#   aes(x = period_hours, y = power)
# ) +
#   geom_line() +
#   geom_vline(xintercept = 12.42, color = "red", linetype = "dashed") +
#   geom_vline(xintercept = 24.84, color = "blue", linetype = "dashed") +
#   labs(
#     title = "FFT of Drakes Estero Water Temperature",
#     x = "Period (hours)",
#     y = "Power"
#   ) +
#   theme_minimal()
# 
# plot2
# 
# # Salinity data
# dfFFTSal <- dfDrakesSal[, c("DateTime", "calculatedSalPSU")]
# dfFFTSal <- dfFFTSal[
#   !is.na(dfFFTSal$DateTime) &
#     !is.na(dfFFTSal$calculatedSalPSU) &
#     dfFFTSal$calculatedSalPSU != "Not Recorded" &
#     dfFFTSal$calculatedSalPSU != "NA",
# ]
# 
# dfFFTSal$calculatedSalPSU <- as.numeric(dfFFTSal$calculatedSalPSU)
# 
# dfFFTSal <- dfFFTSal[order(dfFFTSal$DateTime), ]
# 
# dfFFTSal <- aggregate(
#   calculatedSalPSU ~ DateTime,
#   data = dfFFTSal,
#   FUN = mean
# )
# 
# ySal <- dfFFTSal$calculatedSalPSU
# ySal_detrended <- ySal - mean(ySal, na.rm = TRUE)
# 
# dt_hoursSal <- as.numeric(
#   median(diff(dfFFTSal$DateTime)),
#   units = "hours"
# )
# 
# fft_resultSal <- fft(ySal_detrended)
# 
# nSal <- length(ySal_detrended)
# freqSal <- (0:(nSal - 1)) / (nSal * dt_hoursSal)
# powerSal <- Mod(fft_resultSal)^2
# 
# dfFFTSalResult <- data.frame(
#   frequency_cph = freqSal[1:floor(nSal / 2)],
#   period_hours = 1 / freqSal[1:floor(nSal / 2)],
#   power = powerSal[1:floor(nSal / 2)]
# )
# 
# dfFFTSalResult <- dfFFTSalResult[
#   is.finite(dfFFTSalResult$period_hours),
# ]
# 
# plot3 <- ggplot(
#   dfFFTSalResult[dfFFTSalResult$period_hours < 40, ],
#   aes(x = period_hours, y = power)
# ) +
#   geom_line() +
#   geom_vline(xintercept = 12.42, color = "red", linetype = "dashed") +
#   geom_vline(xintercept = 24.84, color = "blue", linetype = "dashed") +
#   labs(
#     title = "FFT of Drakes Estero Conductivity",
#     x = "Period (hours)",
#     y = "Power"
#   ) +
#   theme_minimal()
# plot3
# 
# # O2 data
# dfFFTO2 <- dfDrakesO2[, c("DateTime", "calculatedDOPct")]
# dfFFTO2 <- dfFFTO2[
#   !is.na(dfFFTO2$DateTime) &
#     !is.na(dfFFTO2$calculatedDOPct) &
#     dfFFTO2$calculatedDOPct != "Not Recorded" &
#     dfFFTO2$calculatedDOPct != "NA",
# ]
# 
# dfFFTO2$calculatedDOPct <- as.numeric(dfFFTO2$calculatedDOPct)
# 
# dfFFTO2 <- dfFFTO2[order(dfFFTO2$DateTime), ]
# 
# # Average duplicate timestamps
# dfFFTO2 <- aggregate(
#   calculatedDOPct ~ DateTime,
#   data = dfFFTO2,
#   FUN = mean
# )
# 
# # Check time spacing
# table(diff(dfFFTO2$DateTime))
# 
# yO2 <- dfFFTO2$calculatedDOPct
# yO2_detrended <- yO2 - mean(yO2, na.rm = TRUE)
# 
# dt_hoursO2 <- as.numeric(
#   median(diff(dfFFTO2$DateTime)),
#   units = "hours"
# )
# 
# fft_resultO2 <- fft(yO2_detrended)
# 
# nO2 <- length(yO2_detrended)
# freqO2 <- (0:(nO2 - 1)) / (nO2 * dt_hoursO2)
# powerO2 <- Mod(fft_resultO2)^2
# 
# dfFFTO2Result <- data.frame(
#   frequency_cph = freqO2[1:floor(nO2 / 2)],
#   period_hours = 1 / freqO2[1:floor(nO2 / 2)],
#   power = powerO2[1:floor(nO2 / 2)]
# )
# 
# dfFFTO2Result <- dfFFTO2Result[
#   is.finite(dfFFTO2Result$period_hours),
# ]
# 
# plot4 <- ggplot(
#   dfFFTO2Result[dfFFTO2Result$period_hours < 40, ],
#   aes(x = period_hours, y = power)
# ) +
#   geom_line() +
#   geom_vline(xintercept = 12.42, color = "red", linetype = "dashed") +
#   geom_vline(xintercept = 24.84, color = "blue", linetype = "dashed") +
#   labs(
#     title = "FFT of Drakes Estero Raw Dissolved O2",
#     x = "Period (hours)",
#     y = "Power"
#   ) +
#   theme_minimal()
# 
# plot4
# 
# 
# 
# plot1/plot2/plot3/plot4
# 
# ###Zoom in on 5-10 hr region
# 
# intMarker <- 8.175
# intStartPeriod <- 5
# intEndPeriod <- 10
# 
# 
# 
# plot11<-ggplot(dfFFTDepthResult[dfFFTDepthResult$period_hours > intStartPeriod&dfFFTDepthResult$period_hours<intEndPeriod, ],
#               aes(x = period_hours, y = power)) +
#   geom_line() +
#   geom_vline(xintercept = intMarker, color = "red", linetype = "dashed") +
#   #geom_vline(xintercept = 24.84, color = "blue", linetype = "dashed") +
#   labs(
#     title = paste0("FFT of Drakes Estero Water Depth ",intStartPeriod,"-",intEndPeriod," Hour Frequency"),
#     x = "Period (hours)",
#     y = "Power"
#   ) +
#   theme_minimal()
# plot11
# 
# 
# 
# plot12 <- ggplot(
#   dfFFTTempResult[dfFFTTempResult$period_hours > intStartPeriod & dfFFTTempResult$period_hours<intEndPeriod,],
#   aes(x = period_hours, y = power)
# ) +
#   geom_line() +
#   geom_vline(xintercept = intMarker, color = "red", linetype = "dashed") +
#   #geom_vline(xintercept = 24.84, color = "blue", linetype = "dashed") +
#   labs(
#     title = paste0("FFT of Drakes Estero Water Temperature ",intStartPeriod,"-",intEndPeriod," Hour Frequency"),
#     x = "Period (hours)",
#     y = "Power"
#   ) +
#   theme_minimal()
# plot12
# 
# plot13 <- ggplot(
#   dfFFTSalResult[
#     dfFFTSalResult$period_hours > intStartPeriod &
#       dfFFTSalResult$period_hours < intEndPeriod,
#   ],
#   aes(x = period_hours, y = power)
# ) +
#   geom_line() +
#   geom_vline(xintercept = intMarker, color = "red", linetype = "dashed") +
#   labs(
#     title = paste0("FFT of Drakes Estero Raw Conductivity ",intStartPeriod,"-",intEndPeriod," Hour Frequency"),
#     x = "Period (hours)",
#     y = "Power"
#   ) +
#   theme_minimal()
# 
# 
# plot14 <- ggplot(
#   dfFFTO2Result[
#     dfFFTO2Result$period_hours > intStartPeriod &
#       dfFFTO2Result$period_hours < intEndPeriod,
#   ],
#   aes(x = period_hours, y = power)
# ) +
#   geom_line() +
#   geom_vline(xintercept = intMarker, color = "red", linetype = "dashed") +
#   labs(
#     title = paste0("FFT of Drakes Estero Raw Dissolved O2 ",intStartPeriod,"-",intEndPeriod," Hour Frequency"),
#     x = "Period (hours)",
#     y = "Power"
#   ) +
#   theme_minimal()
# 
# plot14
# 
# plot1/plot2/plot3/plot4
# plot11/plot12/plot13/plot14
