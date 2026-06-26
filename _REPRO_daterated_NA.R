# _REPRO_daterated_NA.R
# ---------------------------------------------------------------------------
# Reproduction script for the "original date column left NA on filled rows" bug
# in menstrualcycleR::pacts_scaling().
#
# BUG (pre-fix):
#   pacts_scaling() / calculate_mcyclength() internally densify the calendar with
#   tidyr::complete(date = seq.Date(...)) so that every calendar day between a
#   participant's first and last observation gets a row. The internal canonical
#   `date` column is populated on those filled-in rows, but the USER'S ORIGINAL
#   date column (e.g. `daterated`) is left NA. The returned frame therefore has
#   a half-populated original-date column: rows exist with a real calendar date
#   in `date` but NA in `daterated`.
#
#   Impact: imputed ovulation for a participant's FIRST cycle (and any imputed
#   ov / scaled value that lands on a filled day) sits on such a row. A consumer
#   who joins results back by the ORIGINAL date column name silently drops them.
#
# This script is meant to be run from the package root with the package source
# loaded via devtools::load_all(). It prints diagnostics and (pre-fix) stops with
# an error if the bug is present, so it doubles as a manual check.
# ---------------------------------------------------------------------------

suppressMessages({
  library(dplyr)
})

# Load the package from source (R/ on disk), NOT an installed copy.
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  # Fallback: source the R files directly.
  for (f in list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)) sys.source(f, envir = globalenv())
}

# ---------------------------------------------------------------------------
# Small synthetic dataset: ~3 menses-to-menses cycles ~28 days apart over ~90d,
# with NO observed ovulation (ovtoday all 0) so the function imputes ovulation.
# Onsets at days 1, 29, 57, and a closing onset at day 85 (so 3 complete cycles).
#
# CRITICAL for reproducing the bug: the input must have GAPS in the calendar.
# The bug appears only on rows that tidyr::complete() FABRICATES to fill missing
# calendar days -- those rows get a populated internal `date` but NA in the
# user's original `daterated`. A participant who logged a daily diary with some
# missing days (the realistic case) hits this. Here we drop the diary entries on
# the imputed-ovulation days (day 15, 43, 71 -> 14 days before each next onset)
# so the imputed ovulation lands on a fabricated, NA-`daterated` row in cycle 1.
# (With 28-day cycles, follength_impute = 28 - 14 = 14, so ovtoday_impute lands
#  on day 14 of each cycle: global days 14, 42, 70. Dropping those forces the
#  imputed ovulation itself onto an NA-`daterated` fabricated row.)
# ---------------------------------------------------------------------------
make_subject <- function(id, start = "2024-01-01", onsets = c(1, 29, 57, 85),
                         drop_days = c(14, 42, 70)) {
  days  <- seq.Date(as.Date(start), as.Date(start) + max(onsets) - 1, by = "day")
  dayno <- seq_along(days)
  d <- data.frame(
    id        = id,
    daterated = days,
    menses    = as.integer(dayno %in% onsets),
    ovtoday   = 0L,                # no biomarker ovulation -> forces imputation
    stringsAsFactors = FALSE
  )
  # Introduce realistic missing diary days (gaps the function will fill in).
  d[!(dayno %in% drop_days), , drop = FALSE]
}

df <- make_subject("S01")

cat("Input rows:", nrow(df), "(gaps present) | menses onsets at rows:",
    which(df$menses == 1), "\n")
cat("Dropped diary days (these become fabricated rows):",
    "2024-01-14, 2024-02-11, 2024-03-10 (the imputed-ovulation days)\n")

res <- suppressMessages(
  pacts_scaling(df, id = id, date = daterated, menses = menses, ovtoday = ovtoday)
)

cat("\n--- Returned column names ---\n")
print(names(res))

# Does the returned frame carry BOTH `date` and the original `daterated`?
has_date      <- "date" %in% names(res)
has_daterated <- "daterated" %in% names(res)
cat(sprintf("\nHas internal `date`: %s | Has original `daterated`: %s\n",
            has_date, has_daterated))

# --- Symptom A: rows where the canonical date exists but the original is NA ---
if (has_date && has_daterated) {
  bad <- res %>% filter(!is.na(date) & is.na(daterated))
  cat(sprintf("\nRows with a real calendar `date` but NA `daterated`: %d (of %d)\n",
              nrow(bad), nrow(res)))
}

# --- Symptom B: an imputed ovulation lands on an NA-`daterated` row in cycle 1 ---
if ("ovtoday_impute" %in% names(res) && has_daterated) {
  ov_imp <- res %>% filter(ovtoday_impute == 1)
  cat(sprintf("\nImputed ovulation days (ovtoday_impute==1): %d total\n", nrow(ov_imp)))
  ov_imp_na <- ov_imp %>% filter(is.na(daterated))
  cat(sprintf("  ...of which have NA `daterated`: %d\n", nrow(ov_imp_na)))
  if (nrow(ov_imp) > 0) {
    cat("\nImputed-ovulation rows (date, daterated, cyclenum, scaled_cycleday_impute):\n")
    print(ov_imp %>% select(any_of(c("date", "daterated", "cyclenum",
                                     "scaled_cycleday_impute", "cyclic_time_impute"))))
  }
}

# --- Symptom C: share of cyclic_time_impute values sitting on NA-daterated rows ---
if ("cyclic_time_impute" %in% names(res) && has_daterated) {
  ct <- res %>% filter(!is.na(cyclic_time_impute))
  ct_na <- ct %>% filter(is.na(daterated))
  cat(sprintf(
    "\ncyclic_time_impute non-NA rows: %d | of those on NA-`daterated` rows: %d (%.1f%%)\n",
    nrow(ct), nrow(ct_na), 100 * nrow(ct_na) / max(nrow(ct), 1)))
}

# ---------------------------------------------------------------------------
# Assertions: pre-fix these FAIL (bug present); post-fix they PASS.
# ---------------------------------------------------------------------------
cat("\n--- Assertions ---\n")
problems <- character(0)

if (has_date && has_daterated) {
  n_bad <- res %>% filter(!is.na(date) & is.na(daterated)) %>% nrow()
  if (n_bad > 0) problems <- c(problems,
    sprintf("%d rows have a real `date` but NA `daterated`", n_bad))
}

# Any row that carries a real scaled/cycle value must have a non-NA original date.
value_cols <- intersect(
  c("scaled_cycleday", "scaled_cycleday_impute", "scaled_cycleday_ov",
    "scaled_cycleday_imp_ov", "cyclic_time", "cyclic_time_impute",
    "cyclic_time_ov", "cyclic_time_imp_ov", "ovtoday_impute"),
  names(res)
)
if (has_daterated && length(value_cols) > 0) {
  any_value <- Reduce(`|`, lapply(value_cols, function(cn) {
    v <- res[[cn]]
    if (cn == "ovtoday_impute") !is.na(v) & v == 1 else !is.na(v)
  }))
  n_value_na_date <- sum(any_value & is.na(res$daterated))
  if (n_value_na_date > 0) problems <- c(problems,
    sprintf("%d rows carry a scaled/cycle value but have NA `daterated`", n_value_na_date))
}

if (length(problems) == 0) {
  cat("PASS: no NA in the original date column on rows with a real date / value.\n")
} else {
  cat("FAIL (bug present):\n")
  for (p in problems) cat("  - ", p, "\n", sep = "")
}
