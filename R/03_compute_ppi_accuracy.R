## =============================================================================
## Step 3: PPI accuracy - Bland-Altman bias/limits of agreement, by phase
## =============================================================================
#' Matches each correctly-timezone-parsed wearable beat (from step 02) to its
#' nearest reference beat, then summarises per-beat pulse-to-pulse-interval
#' (PPI) error per task phase using Bland-Altman bias + 95% limits of
#' agreement (LoA) as the primary metric, plus MAE and percent-within-
#' tolerance as secondary/descriptive measures. Event-phase is used as the
#' analysis unit (not fixed time windows) because it is what the downstream
#' research question ("how accurate is the wearable during X task") actually
#' needs.
#'
#' Output:
#'   output/tables/ppi_accuracy_by_phase.csv
#'   output/tables/ppi_accuracy_overall.csv
#'   output/tables/ppi_per_beat_errors.csv (feeds step 04's resampling calibration)

library(here)
library(dplyr)
library(readr)
library(purrr)

source(here::here("R", "00_config.R"))

beats_matched <- read_csv(
  here::here("data", "processed", "beats_matched.csv"), show_col_types = FALSE
)
if (nrow(beats_matched) == 0L) {
  stop(
    "data/processed/beats_matched.csv has 0 rows - step 02 didn't match any ",
    "wearable beats to a phase window. Re-run R/02_diagnose_and_fix_timezone_bug.R ",
    "and check its 'AFTER FIX' summary before continuing."
  )
}
## Read timestamp_local as plain character (see the note in step 02) so that
## readr's datetime auto-guessing can't pre-empt the explicit tz = CONFIG$
## timezone parse below.
reference_files <- list.files(here::here("data", "raw", "reference"), full.names = TRUE)
reference_raw <- map_dfr(
  reference_files, read_csv, col_types = cols(.default = col_character())
) %>%
  mutate(
    ppi_ms = as.numeric(ppi_ms),
    timestamp_local_parsed = as.POSIXct(timestamp_local, tz = CONFIG$timezone)
  )

#' Nearest-neighbour match of wearable beats to reference beats within one
#' subject, using findInterval on the (already sorted) reference timestamps.
#' Returns the nearest reference beat regardless of distance; callers filter
#' on gap_s themselves using CONFIG$matching_max_gap_s (see the note next to
#' that config value). Note on findInterval's cost: it does a binary search
#' per query against the sorted reference vector by default, so this scales
#' well, but the exact cost depends on input characteristics (e.g. how
#' sorted/interleaved the two series already are) - not stated as a strict
#' asymptotic guarantee here, since that would depend on implementation
#' details this function doesn't control or verify.
match_nearest <- function(wearable_ts, reference_ts) {
  w <- as.numeric(wearable_ts)
  r <- as.numeric(reference_ts)
  idx <- pmin(pmax(findInterval(w, r), 1L), length(r))
  idx_next <- pmin(idx + 1L, length(r))
  gap_this <- abs(w - r[idx])
  gap_next <- abs(w - r[idx_next])
  use_next <- gap_next < gap_this
  best_idx <- ifelse(use_next, idx_next, idx)
  best_gap <- ifelse(use_next, gap_next, gap_this)
  tibble(reference_idx = best_idx, gap_s = best_gap)
}

per_beat_errors <- beats_matched %>%
  group_by(subject_id) %>%
  group_split() %>%
  map_dfr(function(wear_sub) {
    sid <- wear_sub$subject_id[1]
    ref_sub <- reference_raw %>% filter(subject_id == sid) %>% arrange(timestamp_local_parsed)
    if (nrow(ref_sub) == 0L) return(tibble())
    matched <- match_nearest(wear_sub$timestamp_local_parsed, ref_sub$timestamp_local_parsed)
    wear_sub %>%
      mutate(
        reference_ppi_ms = ref_sub$ppi_ms[matched$reference_idx],
        match_gap_s = matched$gap_s
      ) %>%
      filter(match_gap_s <= CONFIG$matching_max_gap_s) %>%
      mutate(
        signed_error_ms = ppi_ms - reference_ppi_ms,
        abs_error_ms = abs(signed_error_ms)
      )
  })

write_csv(
  per_beat_errors %>% select(subject_id, recording_date, phase_name, signed_error_ms, abs_error_ms),
  here::here("output", "tables", "ppi_per_beat_errors.csv")
)

#' Bland-Altman bias, SD of differences, 95% limits of agreement, MAE, and
#' percent of beats within CONFIG$resampling$tolerance_ms, for one group of
#' per-beat errors.
summarise_accuracy <- function(errors) {
  tol <- CONFIG$resampling$tolerance_ms
  tibble(
    n_beats = nrow(errors),
    bias_ms = mean(errors$signed_error_ms),
    sd_diff_ms = sd(errors$signed_error_ms),
    loa_lower_ms = mean(errors$signed_error_ms) - 1.96 * sd(errors$signed_error_ms),
    loa_upper_ms = mean(errors$signed_error_ms) + 1.96 * sd(errors$signed_error_ms),
    mae_ms = mean(errors$abs_error_ms),
    pct_within_tolerance = 100 * mean(errors$abs_error_ms <= tol)
  )
}

by_phase <- per_beat_errors %>%
  group_by(subject_id, recording_date, phase_name) %>%
  group_modify(~ summarise_accuracy(.x)) %>%
  ungroup()

by_phase_summary <- by_phase %>%
  group_by(phase_name) %>%
  summarise(
    n_recordings = n(),
    median_n_beats = median(n_beats),
    mean_bias_ms = round(mean(bias_ms), 1),
    mean_mae_ms = round(mean(mae_ms), 1),
    mean_pct_within_tolerance = round(mean(pct_within_tolerance), 1),
    .groups = "drop"
  ) %>%
  arrange(match(phase_name, CONFIG$phases))

overall <- summarise_accuracy(per_beat_errors) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

write_csv(by_phase, here::here("output", "tables", "ppi_accuracy_by_recording_and_phase.csv"))
write_csv(by_phase_summary, here::here("output", "tables", "ppi_accuracy_by_phase.csv"))
write_csv(overall, here::here("output", "tables", "ppi_accuracy_overall.csv"))

cat("=== Overall Bland-Altman (all matched beats, all phases) ===\n")
print(overall)
cat("\n=== By phase (mean across recordings) ===\n")
print(by_phase_summary, n = Inf)
