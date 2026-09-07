## =============================================================================
## Step 4: Calibrate "minimum matched beats per phase" via empirical resampling
## =============================================================================
#' Question: below how many matched beats does a phase-level accuracy
#' estimate (bias, SD, %-within-tolerance) become too noisy to trust? Rather
#' than assume normality and use a closed-form formula (SE(mean) = SD/sqrt(n),
#' etc. - which is only as good as its assumptions, and doesn't reflect the
#' actual, possibly skewed error distribution observed in this data), this
#' script answers the question empirically: repeatedly subsample WITHOUT
#' replacement from the real pooled per-beat error distribution at each
#' candidate sample size, and measure how much the resulting estimates
#' actually vary.
#'
#' This is the same approach used in the original project to move its
#' min_matched_beats_per_phase threshold from an arbitrary round number to a
#' data-grounded one.
#'
#' Output:
#'   output/tables/min_beats_resampling_calibration_detail.csv (per phase x n)
#'   output/tables/min_beats_resampling_calibration_summary.csv (averaged over phases)

library(here)
library(dplyr)
library(readr)
library(purrr)

source(here::here("R", "00_config.R"))
set.seed(CONFIG$seed)

per_beat_errors <- read_csv(
  here::here("output", "tables", "ibi_per_beat_errors.csv"), show_col_types = FALSE
)

tol <- CONFIG$resampling$tolerance_ms
n_reps <- CONFIG$resampling$n_reps
min_pool_size <- CONFIG$resampling$min_pool_size

#' For one phase's pooled per-beat errors, subsamples (without replacement)
#' at each candidate n in CONFIG$min_matched_beats_grid, n_reps times each,
#' and reports the empirical standard deviation of the resulting bias/SD/
#' pct-within-tolerance estimates across those replicates - i.e. the
#' empirical standard error of each statistic AT that sample size.
resample_one_phase <- function(phase_errors, phase_name) {
  pool_signed <- phase_errors$signed_error_ms
  pool_abs <- phase_errors$abs_error_ms
  pool_size <- length(pool_signed)

  if (pool_size < min_pool_size) {
    cat("Skipping phase '", phase_name, "': pool too small (n=", pool_size, ").\n", sep = "")
    return(tibble())
  }

  ## Never subsample more than half the pool - otherwise "replicates" overlap
  ## too heavily and the empirical SE understates true sampling variability.
  grid <- CONFIG$min_matched_beats_grid[CONFIG$min_matched_beats_grid <= pool_size / 2]

  map_dfr(grid, function(n_sub) {
    ## Vectorised: draw all n_reps replicate samples at once as an
    ## (n_sub x n_reps) index matrix, then summarise with colMeans/apply
    ## instead of building + row-binding a tibble per replicate.
    idx <- replicate(n_reps, sample.int(pool_size, n_sub, replace = FALSE))
    signed_mat <- matrix(pool_signed[idx], nrow = n_sub)
    abs_mat <- matrix(pool_abs[idx], nrow = n_sub)
    
    rep_bias <- colMeans(signed_mat)
    rep_sd <- apply(signed_mat, 2, sd)
    rep_pct_within <- colMeans(abs_mat <= tol) * 100
    
    tibble(
      phase_name = phase_name,
      n_sub = n_sub,
      empirical_se_bias_ms = round(sd(rep_bias), 2),
      empirical_se_sd_ms = round(sd(rep_sd), 2),
      empirical_se_pct_pp = round(sd(rep_pct_within), 2)
    )
  })
}

calibration <- per_beat_errors %>%
  group_by(phase_name) %>%
  group_split() %>%
  map_dfr(~ resample_one_phase(.x, .x$phase_name[1]))

write_csv(
  calibration, here::here("output", "tables", "min_beats_resampling_calibration_detail.csv")
)

summary_across_phases <- calibration %>%
  group_by(n_sub) %>%
  summarise(
    mean_se_bias_ms = round(mean(empirical_se_bias_ms), 1),
    mean_se_sd_ms = round(mean(empirical_se_sd_ms), 1),
    mean_se_pct_pp = round(mean(empirical_se_pct_pp), 1),
    .groups = "drop"
  ) %>%
  arrange(n_sub)

write_csv(
  summary_across_phases, here::here("output", "tables", "min_beats_resampling_calibration_summary.csv")
)

cat("=== Empirical precision by candidate threshold (averaged across phases) ===\n")
print(summary_across_phases, n = Inf)
cat(
  "\nDecision rule used in the original project: pick the smallest n_sub\n",
  "beyond which precision gains clearly flatten out (diminishing returns),\n",
  "rather than an arbitrary round number or a closed-form formula that\n",
  "assumes normality. Inspect the table above for that elbow, and weigh it\n",
  "against how many recordings/phases the resulting threshold would exclude\n",
  "(see step 03's output) - the original project's final decision balanced\n",
  "both explicitly rather than optimising precision alone.\n",
  sep = ""
)
