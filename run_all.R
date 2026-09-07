## Runs the full demo pipeline end to end, in order.
## Usage: from the repo root, `Rscript run_all.R` (or source() in an R session).

here::i_am("run_all.R")

message(">>> 01: generating synthetic data")
source(here::here("R", "01_generate_synthetic_data.R"))

message("\n>>> 02: diagnosing + fixing the timezone bug")
source(here::here("R", "02_diagnose_and_fix_timezone_bug.R"))

message("\n>>> 03: computing PPI accuracy (Bland-Altman by phase)")
source(here::here("R", "03_compute_ppi_accuracy.R"))

message("\n>>> 04: calibrating min_matched_beats_per_phase via resampling")
source(here::here("R", "04_calibrate_min_beats_resampling.R"))

message("\n>>> 05: making plots")
source(here::here("R", "05_make_plots.R"))

message("\n>>> Done. See output/tables/ and output/plots/.")
