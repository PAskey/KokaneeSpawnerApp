#Okanagan Kokanee Database
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(spwnr)
library(readxl)
library(dplyr)

spawn_counts = read_excel("REBUILD OKANAGAN KOKANEE STREAMSPAWNERS.xlsx", sheet = "Kokanee Stream")
save(spawn_counts, file = "spawn_counts.rda")

#IMPORTANT: Mission Creek spawning channel is included in MIssion data but known as reach E.
#For clarity in App change name of reach E to MISSION SPAWN CHANNEL. It was constructed in 1988.

data$STREAM[data$STREAM == "MISSION"&data$REACH == "E"&data$YEAR>=1988] = "MISSION SPAWN CHANNEL"

Beta_peak = 2.8912799
Beta_TAUC = 0.1666473
Beta_GAUC = 0.1768914

# Calculate peak estimates and peak_percent per stream
spawn_ests <- spawn_counts %>%
  dplyr::group_by(YEAR, STREAM) %>%
  dplyr::summarize(
    peak_est = round(max(NO_LIVE) * Beta_peak),
    .groups = "drop"
  ) %>%
  dplyr::group_by(STREAM) %>%
  dplyr::mutate(
    peak_percent = round(100 * peak_est / max(peak_est, na.rm = TRUE))
  ) %>%
  dplyr::ungroup()

# Create a new data frame with TREND stream
trend_stream <- spawn_ests %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarize(
    peak_percent = round(mean(peak_percent, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    STREAM = "TREND",
    peak_est = NA_real_
  ) %>%
  dplyr::select(YEAR, STREAM, peak_est, peak_percent)

# Combine TREND with original data
spawn_ests <- dplyr::bind_rows(spawn_ests, trend_stream)

save(spawn_ests, file = "spawn_ests.rda")
