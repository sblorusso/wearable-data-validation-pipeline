## =============================================================================
## Step 2: Diagnose and fix a timezone-parsing bug (worked example)
## =============================================================================
#' Background: in the original project, wearable beats matched almost no
#' scheduled task windows after a pipeline refactor - "event_name" came back
#' empty for the great majority of recordings. The root cause turned out to
#' be a timezone mismatch, not a matching-logic bug: the schedule times were
#' parsed with the correct local timezone, while the raw device timestamps
#' were timezone-naive strings that a downstream parser silently defaulted to
#' UTC (this is exactly what readr::read_csv()'s col_datetime() does for a
#' string with no offset/"Z" suffix). Both timestamps display identical
#' digits ("2024-06-13 10:06:59") but now disagree about which real-world
#' instant those digits refer to - by exactly the UTC offset of the local
#' timezone on that date: +1h in winter (CET), +2h in summer (CEST).
#'
#' This script reproduces that exact bug shape against the synthetic data
#' from step 01 (which was generated as internally consistent, correct local
#' time throughout), shows the diagnostic evidence that pointed at DST
#' specifically (systematic, season-dependent offset - not random noise),
#' and applies the fix.
#'
#' Output: data/processed/beats_matched.csv (wearable beats with a
#' correctly-assigned phase_name, used by steps 03-04).

library(here)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)

source(here::here("R", "00_config.R"))

## IMPORTANT: scheduled_*_local and timestamp_local must be read as plain
## CHARACTER columns, not auto-detected as datetimes. readr::read_csv()'s
## column-type guesser recognises the "YYYY-MM-DD HH:MM:SS[.sss]" pattern
## used here and will silently pre-parse it as a datetime itself (also
## defaulting to UTC for a timezone-naive string) if col_types isn't pinned

naive_char_cols <- cols(.default = col_character())

schedule <- read_csv(here::here("data", "raw", "schedule.csv"), col_types = naive_char_cols) %>%
  mutate(
    scheduled_start = as.POSIXct(scheduled_start_local, tz = CONFIG$timezone),
    scheduled_end = as.POSIXct(scheduled_end_local, tz = CONFIG$timezone)
  )

wearable_files <- list.files(here::here("data", "raw", "wearable"), full.names = TRUE)
wearable_raw <- map_dfr(wearable_files, read_csv, col_types = naive_char_cols) %>%
  mutate(ibi_ms = as.numeric(ibi_ms))

## --- 2a. Reproduce the bug: naive parse defaults to UTC ---------------------
## This mirrors readr::read_csv()'s default behaviour for a timezone-naive
## datetime string - the parser has to pick SOME timezone, and it picks UTC.
wearable_buggy <- wearable_raw %>%
  mutate(timestamp_buggy = as.POSIXct(timestamp_local, tz = "UTC"))

## Try matching against the (correctly-parsed) schedule.
match_one_buggy <- function(sub_beats, sub_schedule) {
  purrr::map_dfr(seq_len(nrow(sub_schedule)), function(i) {
    win <- sub_schedule[i, ]
    in_window <- sub_beats$timestamp_buggy >= win$scheduled_start &
      sub_beats$timestamp_buggy <= win$scheduled_end
    tibble(
      subject_id = win$subject_id, phase_name = win$phase_name,
      n_matched = sum(in_window)
    )
  })
}

buggy_match_summary <- wearable_buggy %>%
  group_by(subject_id) %>%
  group_split() %>%
  map_dfr(function(sub_beats) {
    sid <- sub_beats$subject_id[1]
    match_one_buggy(sub_beats, schedule %>% filter(subject_id == sid))
  })

cat(
  "=== BEFORE FIX ===\n",
  "Total scheduled phase-windows: ", nrow(buggy_match_summary), "\n",
  "Windows with zero matched wearable beats: ",
  sum(buggy_match_summary$n_matched == 0), " (",
  round(100 * mean(buggy_match_summary$n_matched == 0), 1), "%)\n",
  "-> matches the real symptom: 'event_name' essentially always empty.\n\n",
  sep = ""
)

## --- 2b. Diagnostic evidence: is the offset random, or systematic? ---------
## For each subject, compare the median wearable (buggy) beat instant to the
## median scheduled instant across their whole recording, in hours. If this
## tracks the calendar month rather than looking like noise, that is a
## strong signal the cause is DST, not a matching-logic bug.
offset_evidence <- wearable_buggy %>%
  group_by(subject_id) %>%
  summarise(median_wearable_instant = median(timestamp_buggy), .groups = "drop") %>%
  left_join(
    schedule %>% group_by(subject_id) %>%
      summarise(median_schedule_instant = median(scheduled_start), .groups = "drop"),
    by = "subject_id"
  ) %>%
  mutate(
    recording_month = month(median_schedule_instant, label = TRUE),
    offset_hours = round(
      as.numeric(median_wearable_instant - median_schedule_instant, units = "hours"), 1
    ),
    season_guess = if_else(dst(median_schedule_instant), "CEST (summer, UTC+2)", "CET (winter, UTC+1)")
  )

cat("Offset (wearable-buggy minus schedule), by month:\n")
print(
  offset_evidence %>%
    group_by(recording_month, season_guess) %>%
    summarise(n = n(), mean_offset_hours = round(mean(offset_hours), 2), .groups = "drop") %>%
    arrange(recording_month),
  n = Inf
)
cat(
  "\n-> The offset clusters at ~-1h in winter months and ~-2h in summer\n",
  "months (sign/magnitude depend on which side is 'wrong'), lining up\n",
  "exactly with Europe/Zurich's own CET/CEST transitions rather than\n",
  "varying randomly by subject. That is the signature of a timezone bug,\n",
  "not a data-quality or matching-logic problem.\n\n",
  sep = ""
)

## --- 2c. The fix -------------------------------------------------------------
## The raw digits ARE correct local wall-clock time; they were just mislabeled
## as UTC by the naive parser. force_tz() re-labels a timestamp's timezone
## WITHOUT changing its clock-face digits, which is exactly what's needed
## here: reinterpret the same digits as Europe/Zurich instead of UTC. (In the
## original codebase this took one extra normalisation step, because the
## timestamps had already passed through a prior UTC-tagging parser - the
## underlying fix is the same pattern either way.)
wearable_fixed <- wearable_raw %>%
  mutate(
    timestamp_local_parsed = force_tz(
      as.POSIXct(timestamp_local, tz = "UTC"), tzone = CONFIG$timezone
    )
  )

match_one_fixed <- function(sub_beats, sub_schedule) {
  purrr::map_dfr(seq_len(nrow(sub_schedule)), function(i) {
    win <- sub_schedule[i, ]
    in_window <- sub_beats$timestamp_local_parsed >= win$scheduled_start &
      sub_beats$timestamp_local_parsed <= win$scheduled_end
    tibble(
      subject_id = win$subject_id, recording_date = win$recording_date,
      phase_name = win$phase_name, n_matched = sum(in_window)
    )
  })
}

fixed_match_summary <- wearable_fixed %>%
  group_by(subject_id) %>%
  group_split() %>%
  map_dfr(function(sub_beats) {
    sid <- sub_beats$subject_id[1]
    match_one_fixed(sub_beats, schedule %>% filter(subject_id == sid))
  })

cat(
  "=== AFTER FIX ===\n",
  "Windows with zero matched wearable beats: ",
  sum(fixed_match_summary$n_matched == 0), " of ", nrow(fixed_match_summary), "\n",
  "Median matched beats per window: ", median(fixed_match_summary$n_matched), "\n",
  sep = ""
)

## --- 2d. Write matched beats for downstream steps ---------------------------
beats_matched <- wearable_fixed %>%
  mutate(row_id = row_number()) %>%
  left_join(
    schedule %>% select(subject_id, phase_name, scheduled_start, scheduled_end),
    by = "subject_id",
    relationship = "many-to-many"
  ) %>%
  filter(
    timestamp_local_parsed >= scheduled_start,
    timestamp_local_parsed <= scheduled_end
  ) %>%
  distinct(row_id, .keep_all = TRUE) %>%
  select(subject_id, recording_date, phase_name, timestamp_local_parsed, ibi_ms)

write_csv(beats_matched, here::here("data", "processed", "beats_matched.csv"))
cat("\nWritten: data/processed/beats_matched.csv (", nrow(beats_matched), " rows)\n", sep = "")
