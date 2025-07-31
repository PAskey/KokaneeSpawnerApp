#Okanagan Kokanee Database
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(spwnr)
library(readxl)
library(dplyr)

spawn_counts = read_excel("REBUILD OKANAGAN KOKANEE STREAMSPAWNERS.xlsx", sheet = "Kokanee")
save(spawn_counts, file = "spawn_counts.rda")

#IMPORTANT: Mission Creek spawning channel is included in MIssion data but known as reach E.
#For clarity in App change name of reach E to MISSION SPAWN CHANNEL. It was constructed in 1988.
spawn_counts$STREAM[spawn_counts$STREAM == "MISSION"&spawn_counts$REACH == "E"&spawn_counts$YEAR>=1988] = "MISSION CHANNEL"

Beta_peak = 2.8912799
Beta_TAUC = 0.1666473
Beta_GAUC = 0.1768914

# Calculate peak estimates and peak_percent per stream
spawn_ests <- spawn_counts %>%
  dplyr::group_by(ECOTYPE, LAKE, YEAR, QUADRANT, STREAM, DOY) %>%
  dplyr::summarize(COUNT = sum(NO_LIVE, na.rm = TRUE))
 
    
    
spawn_ests <- spawn_ests %>% 
  dplyr::group_by(ECOTYPE) %>%
  dplyr::group_by(LAKE, YEAR, QUADRANT, STREAM, .add = TRUE) %>%
  dplyr::summarize(
    peak_count = max(COUNT, na.rm = TRUE),
    peak_est = if (ECOTYPE[1] == "STREAM") {
      round( peak_count* Beta_peak)
    } else {
      peak_count
    },
    .groups = "drop"
  ) %>%
  dplyr::group_by(ECOTYPE, LAKE, QUADRANT, STREAM) %>%
  dplyr::mutate(
    peak_percent = round(100 * peak_est / max(peak_est, na.rm = TRUE))
  ) %>%
  dplyr::ungroup()


# Create a new data frame with TREND stream
trend_stream <- spawn_ests %>%
  dplyr::group_by(ECOTYPE, LAKE, YEAR) %>%
  dplyr::summarize(
    peak_percent = round(mean(peak_percent, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    STREAM = "TREND",
    peak_count = NA_real_,
    peak_est = NA_real_
  ) %>%
  dplyr::select(ECOTYPE, LAKE, YEAR, STREAM, peak_est, peak_percent)

# Combine TREND with original data
spawn_ests <- dplyr::bind_rows(spawn_ests, trend_stream)

save(spawn_ests, file = "spawn_ests.rda")
