## =============================================================================
## Config: Wearable Heart-Rate Validation Pipeline (Portfolio Demo)
## =============================================================================
#' This is a self-contained, anonymized demonstration of an inter-beat-
#' interval (IBI) accuracy validation pipeline for a wrist-worn heart-rate
#' sensor against a reference sensor, distilled from a real applied-research
#' project. ALL DATA HERE IS SYNTHETIC - no real subjects, dates, or
#' measurements are included anywhere in this repository.
#'
#' Two engineering decisions from the original project are reproduced here as
#' worked, runnable examples:
#'   1. Diagnosing and fixing a timezone-parsing bug that silently misaligned
#'      device beats with scheduled task windows (02_diagnose_and_fix_timezone_bug.R).
#'   2. Calibrating a "minimum matched beats per phase" quality threshold via
#'      empirical resampling instead of a closed-form normal-approximation
#'      formula (04_calibrate_min_beats_resampling.R).
#'
#' See README.md for how to run this, and docs/case_study.md for the full
#' narrative behind the timezone bug.

library(here)

CONFIG <- list(
  seed = 20260907,
  n_subjects = 24,
  ## Recording dates are spread across the year on purpose - this is what
  ## makes the timezone bug in step 02 visible at all: its size depends on
  ## whether Central European Summer Time (CEST, UTC+2) or Central European
  ## Time (CET, UTC+1) is in effect on a given date.
  recording_dates = as.Date(character(0)), # filled in 01_generate_synthetic_data.R
  phases = c(
    "Baseline_Rest", "Cognitive_Task", "Stressor_1", "Stressor_2", "Novel_Stimulus"
  ),
  phase_duration_seconds = c(300, 240, 240, 240, 180),
  ## Rough between-phase gap while the experimenter resets the task.
  inter_phase_gap_seconds = c(20, 30, 25, 20),
  mean_ibi_ms = c(
    Baseline_Rest = 750, Cognitive_Task = 700, Stressor_1 = 620,
    Stressor_2 = 600, Novel_Stimulus = 640
  ),
  ibi_within_phase_sd_ms = 45,
  ## Wearable measurement error model: a systematic negative bias (wearable
  ## under-reads IBI slightly, i.e. over-reads heart rate) plus noise, and a
  ## small dropout rate (missed beats -> gaps that a real pipeline would
  ## either exclude ["observed_only"] or bridge ["including_interpolated"]).
  wearable_bias_ms = -18,
  wearable_noise_sd_ms = 35,
  wearable_dropout_rate = 0.05,
  reference_noise_sd_ms = 6,
  ## Grid of candidate thresholds evaluated in the resampling calibration.
  min_matched_beats_grid = c(10, 15, 20, 25, 30, 40, 50, 75, 100, 150, 200),
  resampling = list(
    n_reps = 1000,
    min_pool_size = 150,
    tolerance_ms = 100
  ),
  timezone = "Europe/Zurich",
  dirs = list(
    raw = here::here("data", "raw"),
    processed = here::here("data", "processed"),
    output_tables = here::here("output", "tables"),
    output_plots = here::here("output", "plots")
  )
)

invisible(lapply(CONFIG$dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
