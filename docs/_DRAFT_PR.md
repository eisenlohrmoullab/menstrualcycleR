# DRAFT PR (provisional — for maintainer review before committing/pushing)

## Title

Fix:
[`pacts_scaling()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/pacts_scaling.md)
leaves the user’s original date column `NA` on calendar-filled rows

## Status

**Provisional / conservative.** This is a candidate fix prepared locally
for review. Nothing has been pushed and no PR has been opened. Please
review the diagnosis and the trade-offs (below) before deciding whether
to merge or take a different approach.

## Summary

[`pacts_scaling()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/pacts_scaling.md)
returns a data frame that carries **two** date columns: an internal
canonical `date` and the user’s original date column (e.g. `daterated`).
On any calendar day that the participant did **not** have a row in the
input, the internal `tidyr::complete(date = seq.Date(...))` in
`calculate_mcyclength()` **fabricates** a row. On those fabricated rows
the canonical `date` is populated, but the user’s original date column
is left `NA`.

Because imputed ovulation and scaled/cycle-time values can land on a
fabricated row, a consumer who joins the results back to other data **by
the original date column name** silently drops those rows. On the
bundled `cycledata`, the input has 619 rows and the returned frame has
744 — i.e. **125 fabricated rows** that previously had `NA` in
`daterated`.

## Root cause

- `R/pacts_scaling.R` (pre-fix L66–72): creates a canonical `date`
  column as a copy of the user’s `date` argument, so both `date` and
  (e.g.) `daterated` exist.
- `R/calculate_mcyclength.r` L72–82:
  `tidyr::complete(!!date := seq.Date(min, max, by="day"))` fills the
  calendar.
  [`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html)
  only populates the *completion* variable (`date`) and `id` (restored
  by the explicit
  [`tidyr::fill()`](https://tidyr.tidyverse.org/reference/fill.html) on
  L81). Every other carried-along column — including the user’s original
  date column — is `NA` on the new rows. The impute helpers in
  `R/helper.R` do **not** call `complete()`, so this
  [`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html)
  is the single source of fabricated rows.

Verified empirically (see `_REPRO_daterated_NA.R`): a first-cycle
imputed ovulation (`ovtoday_impute == 1`) had `daterated == NA` while
`date` was correct (`2024-01-14`), and the affected rows carried real
`cyclic_time_impute` / `scaled_cycleday_impute` values.

## What changed

One block added to `R/pacts_scaling.R`, after the two compute calls:

``` r
date_name <- rlang::as_name(date)
if (date_name != "date" && date_name %in% names(data)) {
  data <- data %>%
    dplyr::mutate(!!date := dplyr::coalesce(!!date, date))
}
```

This refills the user’s original date column from the authoritative
`date` wherever it is `NA` (i.e. only on fabricated rows), keeping both
date columns internally consistent. It is skipped when the user passed
`date = date` (the original column *is* the canonical one). `coalesce`
is used rather than an unconditional overwrite so that any
genuinely-observed original-date value is never disturbed.

**This is the only behavioural change. No scaled/cycle value, no other
column, and the column-name contract are touched** — the `mutate`
rewrites only the original date column, and only on rows where it was
`NA`.

New regression test:
`tests/testthat/test_daterated_na_on_filled_rows.R`.

## How to verify

From the package root (the repo uses `renv`; these commands use the
system library by disabling the renv autoloader — adjust to your setup
as needed):

``` sh
# 1. Reproduce + confirm the fix (script prints PASS post-fix, FAIL pre-fix):
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE Rscript _REPRO_daterated_NA.R

# 2. Run the test suite:
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE Rscript -e \
  'devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

Result on this machine: **15 passing, 0 failing, 0 warnings** (the one
“skip” is the pre-existing fully-commented-out
`test_process_follicular_phase.R`, unrelated).

On the bundled `cycledata`: post-fix, 0 rows have a real `date` but `NA`
`daterated`, and 0 scaled/cycle values sit on an `NA`-`daterated` row
(pre-fix: 125 fabricated rows carried `NA` `daterated`).

## Alternatives considered (and why option (a) was chosen)

- **(a) Refill the original date column from `date` (CHOSEN).**
  Preserves the existing column-name contract (both `date` and
  `daterated` remain), guarantees internal consistency, minimal surface
  area, no change to any computed value. Trade-off: the frame still
  carries two identical date columns (mild redundancy).
- **(b) Drop the redundant original column and return a single canonical
  `date`.** Cleanest output, but **breaks the column-name contract** —
  existing user code that refers to `daterated` (or whatever they
  passed) would stop finding it. Rejected as too disruptive for a
  bug-fix release.
- **(c) Document `date` as authoritative and leave the original column
  as-is.** Lowest code change, but leaves a half-populated column in the
  output — the silent join-drop footgun remains for anyone who keys on
  the original name. Rejected.

A deeper structural alternative would be to fix the un-filled column
*inside* `calculate_mcyclength()` (e.g. fill the original date column
right after
[`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html)).
It was **not** chosen because, inside that helper, the quosure already
resolves to the canonical `date` column (`pacts_scaling` overwrites it
before the call), so the helper does not cleanly know the user’s
original column name. Doing the refill in
[`pacts_scaling()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/pacts_scaling.md),
where the original date quosure is in scope, is the more direct and
lower-risk place.

## Open questions for the maintainer

1.  **Two date columns by design?** If the canonical `date` is meant to
    be the single public output, option (b) (drop the original) may be
    preferable for a future major version — but that’s a breaking
    change. Keeping both (this PR) is the non-breaking choice.
2.  **Other entry points.** `calculate_mcyclength()` /
    `calculate_cycletime()` are `@noRd` internal helpers, but if any are
    ever exported or called directly, they would still return the
    un-filled original column. Should the refill live in the helper
    instead (despite the column-name awkwardness), to cover all paths?
3.  **Non-date passenger columns.**
    [`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html)
    also leaves any *other* user-supplied per-row columns (covariates,
    symptoms) `NA` on fabricated rows. That is arguably correct (those
    days genuinely had no observation), but worth a docs note so users
    know fabricated rows exist and only `date`-derived fields are
    filled.

## Files

- `R/pacts_scaling.R` — the fix.
- `tests/testthat/test_daterated_na_on_filled_rows.R` — regression test.
- `_REPRO_daterated_NA.R` — standalone reproduction / manual check (repo
  root; not part of the package build — add to `.Rbuildignore` or remove
  before release if undesired).
