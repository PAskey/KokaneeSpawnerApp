#Okanagan Kokanee Database
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
devtools::install_github("PAskey/spwnr")


library(spwnr)
library(readxl)
library(dplyr)

spawn_counts = read_excel("OKANAGAN KOKANEE SPAWNERS CANONICAL.xlsx", sheet = "Kokanee")
# DATE is an Excel formula; files saved by scripts (not Excel) carry no cached value, so rebuild it here
spawn_counts$DATE = as.Date(ISOdate(spawn_counts$YEAR, spawn_counts$MONTH, spawn_counts$DAY))
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


# ---- Site share table ---------------------------------------------------------
# Average share of each lake x ecotype total contributed by each site (stream, or
# shore quadrant). Use it to judge whether a site missed in a given year matters:
# missing a site with a share of a few % is within the observation error of the
# major sites, but missing a MAJOR site is worth noting for that year's trend.
#   share             = site's mean peak_est / sum of all sites' mean peak_est
#                       (main measure; not inflated by years with missed sites)
#   mean_annual_share = mean over surveyed years of site peak_est / that year's
#                       lake x ecotype total (inflated in poorly covered years)
#   cum_share         = running total of share, largest site first
site_share_start_year <- 2010  # reference period start; shares shift over time
site_share_major      <- 0.05  # share at or above which a site counts as MAJOR

site_shares <- spawn_ests %>%
  dplyr::filter(is.na(STREAM) | STREAM != "TREND") %>%
  dplyr::mutate(SITE = dplyr::if_else(is.na(STREAM), QUADRANT, STREAM)) %>%
  dplyr::filter(!is.na(SITE), SITE != "UNK", YEAR >= site_share_start_year) %>%
  dplyr::group_by(LAKE, ECOTYPE, YEAR) %>%
  dplyr::mutate(year_share = peak_est / sum(peak_est, na.rm = TRUE)) %>%
  dplyr::group_by(LAKE, ECOTYPE, SITE) %>%
  dplyr::summarize(
    n_years           = dplyr::n_distinct(YEAR),
    first_year        = min(YEAR),
    last_year         = max(YEAR),
    mean_peak_est     = mean(peak_est, na.rm = TRUE),
    mean_annual_share = mean(year_share[is.finite(year_share)]),
    .groups = "drop_last"
  ) %>%
  dplyr::mutate(share = mean_peak_est / sum(mean_peak_est, na.rm = TRUE)) %>%
  dplyr::arrange(dplyr::desc(share), .by_group = TRUE) %>%
  dplyr::mutate(
    cum_share = cumsum(share),
    MAJOR     = share >= site_share_major
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    mean_peak_est = round(mean_peak_est),
    dplyr::across(c(share, mean_annual_share, cum_share), ~ round(.x, 3))
  ) %>%
  dplyr::select(LAKE, ECOTYPE, SITE, share, MAJOR, cum_share, mean_annual_share,
                mean_peak_est, n_years, first_year, last_year)

save(site_shares, file = "site_shares.rda")
