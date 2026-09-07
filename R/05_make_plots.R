## =============================================================================
## Step 5: Plots - Bland-Altman diagram and resampling calibration curve
## =============================================================================
#' Produces the two figures that best summarise this demo:
#'   1. A classic Bland-Altman plot (mean IBI vs. wearable-reference
#'      difference), coloured by task phase, with bias + 95% limits of
#'      agreement.
#'   2. The resampling calibration curve from step 04: empirical SE(bias) as
#'      a function of candidate minimum-beats threshold, per phase - the
#'      figure that actually drove the threshold decision.
#'
#' Output:
#'   output/plots/bland_altman_by_phase.png
#'   output/plots/resampling_calibration_curve.png

library(here)
library(dplyr)
library(readr)
library(ggplot2)

source(here::here("R", "00_config.R"))

per_beat_errors <- read_csv(
  here::here("output", "tables", "ibi_per_beat_errors.csv"), show_col_types = FALSE
)
overall <- read_csv(here::here("output", "tables", "ibi_accuracy_overall.csv"), show_col_types = FALSE)
calibration <- read_csv(
  here::here("output", "tables", "min_beats_resampling_calibration_detail.csv"),
  show_col_types = FALSE
)

# --- Plot 1: Bland-Altman ----------------------------------------------------
## The per-beat error file only stores the wearable-reference difference, not
## the pair mean - so this uses the classic Bland-Altman y-axis (the
## difference) against phase on the x-axis, which keeps the plot
## self-contained without re-reading raw IBI values from two more files.
per_beat_errors <- per_beat_errors %>%
  mutate(phase_name = factor(phase_name, levels = CONFIG$phases))

p_bland_altman <- ggplot(per_beat_errors, aes(x = phase_name, y = signed_error_ms, colour = phase_name)) +
  geom_jitter(width = 0.25, height = 0, alpha = 0.12, size = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = overall$bias_ms, linetype = "solid", colour = "black", linewidth = 0.5) +
  geom_hline(
    yintercept = c(overall$loa_lower_ms, overall$loa_upper_ms),
    linetype = "dashed", colour = "grey35", linewidth = 0.5
  ) +
  labs(
    title = "Bland-Altman: wearable minus reference IBI, by task phase",
    subtitle = paste0(
      "Overall bias = ", overall$bias_ms, " ms (solid line); ",
      "95% limits of agreement = [", overall$loa_lower_ms, ", ", overall$loa_upper_ms, "] ms (dashed)"
    ),
    x = NULL, y = "Wearable - reference IBI (ms)"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(
  here::here("output", "plots", "bland_altman_by_phase.png"),
  p_bland_altman, width = 9, height = 6, dpi = 300
)

# --- Plot 2: resampling calibration curve ------------------------------------
p_calibration <- ggplot(calibration, aes(x = n_sub, y = empirical_se_bias_ms, colour = phase_name)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.6) +
  labs(
    title = "Empirical precision of the phase-level bias estimate vs. sample size",
    subtitle = paste0(
      "Empirical SE(bias) from ", CONFIG$resampling$n_reps,
      " without-replacement resamples per point - the basis for choosing",
      " a minimum-beats threshold"
    ),
    x = "Candidate minimum matched beats per phase (n_sub)",
    y = "Empirical SE of phase-level bias estimate (ms)",
    colour = "Phase"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  here::here("output", "plots", "resampling_calibration_curve.png"),
  p_calibration, width = 9, height = 6.5, dpi = 300
)

cat(
  "Saved:\n",
  " output/plots/bland_altman_by_phase.png\n",
  " output/plots/resampling_calibration_curve.png\n",
  sep = ""
)
