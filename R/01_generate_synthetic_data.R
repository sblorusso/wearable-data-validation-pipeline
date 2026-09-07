## ******************************************************************************
## Step 1: Generate synthetic per-beat data for a wearable + reference sensor
## ******************************************************************************
#' Produces two independent per-beat time series (reference "ground truth"
#' sensor and a wearable with realistic measurement error: bias, noise,
#' dropout) plus a task-phase schedule, for a set of synthetic subjects whose
#' recording dates are deliberately spread across the year. That spread is
#' what makes the timezone bug in step 02 visible: its size depends on
#' whether CEST (summer, UTC+2) or CET (winter, UTC+1) applies on a given
#' date.
#'
#' All timestamps are written as timezone-naive local wall-clock strings
#' (e.g. "2024-06-13 10:06:59.412"), matching how the original device
#' exports looked - this is a deliberate, load-bearing detail for step 02.
#'
#' Output (written under data/raw/, not tracked as "real" data - delete and
#' regenerate freely):
#'   data/raw/reference/{subject_id}.csv
#'   data/raw/wearable/{subject_id}.csv
#'   data/raw/schedule.csv

library(here)
library(dplyr)
library(purrr)
library(readr)

source(here::here("R", "00_config.R"))
set.seed(CONFIG$seed)

dir.create(here::here("data", "raw", "reference"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("data", "raw", "wearable"), recursive = TRUE, showWarnings = FALSE)

subject_ids <- sprintf("P%03d", seq_len(CONFIG$n_subjects))

## Spread recording dates across the full year so both CET and CEST dates
## are represented (roughly evenly, without replacement).
all_year_dates <- seq(as.Date("2024-01-15"), as.Date("2024-12-15"), by = "day")
recording_dates <- sample(all_year_dates, CONFIG$n_subjects, replace = FALSE)

#' Formats a POSIXct as a timezone-NAIVE local wall-clock string, mimicking
#' the raw device export format (no timezone offset, no "Z" suffix).
format_naive <- function(posix_local) {
  format(posix_local, "%Y-%m-%d %H:%M:%OS3")
}

#' Simulates one subject's recording: a phase schedule plus reference and
#' wearable per-beat series for that schedule.
simulate_one_subject <- function(subject_id, recording_date) {

  ## Recording start time: a plausible late-morning testing slot.
  start_hour <- sample(8:11, 1)
  start_minute <- sample(0:59, 1)
  start_local <- as.POSIXct(
    paste(recording_date, sprintf("%02d:%02d:00", start_hour, start_minute)),
    tz = CONFIG$timezone
  )

  n_phases <- length(CONFIG$phases)
  phase_start <- vector("list", n_phases)
  cursor <- start_local

  schedule_rows <- vector("list", n_phases)
  reference_rows <- vector("list", n_phases)
  wearable_rows <- vector("list", n_phases)

  for (p in seq_len(n_phases)) {
    phase_name <- CONFIG$phases[p]
    duration_s <- CONFIG$phase_duration_seconds[p]
    phase_start_time <- cursor
    phase_end_time <- phase_start_time + duration_s

    schedule_rows[[p]] <- tibble(
      subject_id = subject_id,
      recording_date = as.character(recording_date),
      phase_name = phase_name,
      scheduled_start_local = format_naive(phase_start_time),
      scheduled_end_local = format_naive(phase_end_time)
    )

    ## --- Reference sensor: "ground truth" beats for this phase ------------
    mean_ppi <- CONFIG$mean_ppi_ms[[phase_name]]
    ## Generate slightly more beats than fit in the window, then trim -
    ## simpler than solving for exact beat count up front.
    n_beats_guess <- ceiling(duration_s * 1000 / mean_ppi) + 20
    ref_ppi_ms <- pmax(
      300, rnorm(n_beats_guess, mean = mean_ppi, sd = CONFIG$ppi_within_phase_sd_ms)
    )
    ref_elapsed_s <- cumsum(ref_ppi_ms) / 1000
    ref_elapsed_s <- ref_elapsed_s[ref_elapsed_s <= duration_s]
    ref_timestamps <- phase_start_time + ref_elapsed_s
    
    reference_rows[[p]] <- tibble(
      subject_id = subject_id,
      recording_date = as.character(recording_date),
      timestamp_local = format_naive(ref_timestamps),
      ppi_ms = ref_ppi_ms[seq_along(ref_elapsed_s)]
    )
    
    ## --- Wearable: independent noisy series + dropout ----------------------
    wear_ppi_ms <- pmax(
      300,
      ref_ppi_ms[seq_along(ref_elapsed_s)] +
        rnorm(length(ref_elapsed_s), mean = CONFIG$wearable_bias_ms, sd = CONFIG$wearable_noise_sd_ms)
    )
    wear_elapsed_s <- cumsum(wear_ppi_ms) / 1000
    keep <- wear_elapsed_s <= duration_s &
      runif(length(wear_elapsed_s)) > CONFIG$wearable_dropout_rate
    wear_timestamps <- phase_start_time + wear_elapsed_s[keep]
    
    wearable_rows[[p]] <- tibble(
      subject_id = subject_id,
      recording_date = as.character(recording_date),
      timestamp_local = format_naive(wear_timestamps),
      ppi_ms = wear_ppi_ms[keep]
    )

    ## Advance cursor past this phase plus a short inter-phase gap.
    gap_s <- if (p <= length(CONFIG$inter_phase_gap_seconds)) {
      CONFIG$inter_phase_gap_seconds[p]
    } else {
      20
    }
    cursor <- phase_end_time + gap_s
  }

  list(
    schedule = bind_rows(schedule_rows),
    reference = bind_rows(reference_rows),
    wearable = bind_rows(wearable_rows)
  )
}

simulated <- map2(subject_ids, recording_dates, simulate_one_subject)

schedule_all <- map_dfr(simulated, "schedule")
write_csv(schedule_all, here::here("data", "raw", "schedule.csv"))

walk2(subject_ids, simulated, function(sid, sim) {
  write_csv(sim$reference, here::here("data", "raw", "reference", paste0(sid, ".csv")))
  write_csv(sim$wearable, here::here("data", "raw", "wearable", paste0(sid, ".csv")))
})

cat(
  "Generated synthetic data for ", length(subject_ids), " subjects.\n",
  "Recording dates span ", format(min(recording_dates)), " to ",
  format(max(recording_dates)), " (both CET and CEST represented).\n",
  "Written to data/raw/{reference,wearable}/*.csv and data/raw/schedule.csv\n",
  sep = ""
)
