# menstrualcycleR 0.1.7

Bug fixes that can change results, two new sets of `pacts_scaling()` arguments, input
validation, and a documentation pass. If you use `pacts_scaling()`, re-running under
0.1.7 may change your scaled cycle-time columns — the fixes below say when and how.
(This entry summarizes; the full technical record is in the git history.)

## Fixes that can change your results

* **Post-ovulation days at the end of a participant's record are no longer scaled
  against a made-up cycle end.** When data collection ends after a confirmed
  ovulation but before the next menses (routine at the end of any study), there
  are two intended behaviors: leave those days unscaled in the strict
  observed-anchors-only columns, or — with `impute_next_menses` — impute the
  closing onset at ovulation + 14 days, flagged, and scale against that. Prior
  versions did a third thing nobody designed: they treated **the last observed
  day as if it were the day before menses** and scaled the tail against that
  false cycle end. Example: data ending 6 days after ovulation put day 6 at
  menses-eve instead of mid-luteal (≈ 6/14), day 3 at mid-luteal instead of
  early — the whole tail compressed to fit a "luteal phase" exactly as long as
  the data happened to run. Those wrong positions landed in the strict
  confirmed-only columns (`cyclic_time`, `cyclic_time_ov`, `luteal_length`),
  unflagged, whether or not menses imputation was enabled. They are now `NA`
  there; use `impute_next_menses` to scale such days properly — the closing onset
  is imputed forward at ovulation + 14 days (equivalently LH surge + 15, since
  ovulation is coded LH + 1), and flagged. Present in every prior release;
  affects anyone whose data includes end-of-collection cycles.

* **`POSIXct` dates with a time-of-day no longer silently delete rows.** A date
  column carrying timestamps (e.g. a 09:00 survey time) was coerced to `NA` and
  dropped — a single such timestamp on an ovulation day silently removed that
  ovulation. Dates are now handled correctly, and a genuine parse failure warns
  instead of failing silently.

* **`impute_next_menses` (opt-in, since 0.1.6) no longer fabricates onsets when a
  real closing menses exists.** The search for a closing menses was limited to a
  20-day window, so a real onset just beyond it was missed and a phantom onset was
  written into observed data. The search is now unbounded (limited only by the
  participant's next confirmed ovulation), including the case where the closing
  menses falls exactly on the next ovulation's date. `next_menses_max_window` no
  longer does anything and warns if passed.

* **Days beyond the internal phase-length caps now get values in the imputed
  cycle-time columns instead of silent `NA`.** The package has always restricted
  scaling to luteal phases of 7–18 days and follicular phases of 8–25 (Bull et
  al. 2019 norms). In a confirmed-ovulation cycle whose phase ran past the upper
  cap, the over-cap days had no value in `cyclic_time_impute` or
  `cyclic_time_imp_ov` at all — the confirmed columns correctly excluded them and
  the imputed columns, which exist to cover what the strict ones can't, skipped
  them too. Those days are now filled using the same phase math without the upper
  cap, still restricted to cycles inside your overall cycle-length bounds and to
  phases at or above the minimums (a too-short phase remains excluded, unchanged),
  and flagged in two new columns
  (`cyclic_time_impute_extended_phase`, `cyclic_time_imp_ov_extended_phase`) so you
  can report them or exclude them. The trade-off is deliberate and documented:
  coverage over phase-length plausibility. The strict columns (`cyclic_time`,
  `cyclic_time_ov`) are byte-for-byte unchanged.

## New arguments

* **The phase-length caps are now adjustable:** `luteal_phase_min_days` /
  `luteal_phase_max_days` (defaults 7 / 18) and `follicular_phase_min_days` /
  `follicular_phase_max_days` (defaults 8 / 25), independent of the overall
  cycle-length bounds. Defaults reproduce prior behavior exactly. One asymmetry to
  know: *widening* a cap extends what `cyclic_time` itself covers, but *narrowing*
  one does not make the imputed columns stricter (their fallback ignores the
  ceiling). Details: `?pacts_scaling`, "Internal phase-length caps."

* **`pacts_scaling()` now validates its input.** Missing columns, wrong types, and
  miscoded `menses`/`ovtoday` (e.g. `"yes"`/`"no"`, or bleeding intensity codes like
  `0`/`1`/`2`/`3`) previously ran with no error and returned all-`NA` derived
  columns; they now stop immediately with a message naming the problem. If your
  input errors under 0.1.7 where it "worked" before, it was silently producing
  nothing usable.

## Documentation and housekeeping

* The internal phase caps, the new arguments, and their interaction are documented
  in `?pacts_scaling`; the caps were previously described only in the online PACTS
  explainer, not in the R help. `pacts_scaling()`'s help now documents the full
  return set — the recommended `cyclic_time*` columns, not just the legacy
  `scaled_cycleday*` ones — and the vignette's `impute_next_menses` description was
  corrected. A package-level help topic (`?menstrualcycleR`) was added.
* `R CMD check` is clean (0 errors, 0 warnings, 0 notes). Leftover debug `print()`
  output on every `pacts_scaling()` call removed. `mgcv`, `shinyjs`, and `cpass`
  moved from Imports to Suggests, so installing for `pacts_scaling()` alone is
  lighter. Regression tests added for every fix above.

# menstrualcycleR 0.1.6

* New opt-in argument `impute_next_menses` in `pacts_scaling()` (default `FALSE`).
  When `TRUE`, a next-menses onset is imputed *forward* from a biomarker-confirmed
  ovulation that has no recorded closing menses, placed at `ovulation +
  next_menses_luteal_days` (default 14 days, the population-average luteal length,
  i.e. the "LH+15" / last-follicular-day convention). That cycle then becomes
  scalable instead of being dropped for a missing anchor. Imputed onsets are
  flagged in a new `menses_impute` column so imputed and observed onsets stay
  distinguishable.
* Two companion arguments tune the rule: `next_menses_luteal_days` (default 14)
  and `next_menses_max_window` (default 20; skip imputation for an ovulation if an
  observed menses already closes the cycle within that many days).
* This complements the existing ovulation imputation, which imputes ovulation
  *backward* from an observed menses (menses minus 15). The package can now close a
  cycle from either direction when one anchor is missing.
* The default (`FALSE`) leaves all previous behavior byte for byte identical; only
  callers who opt in see any change. Only the general rule is applied. Study or
  protocol specific gating (for example blocking imputation across treatment phases
  or documented off-study breaks) remains the caller's responsibility.

# menstrualcycleR 0.1.5

* Maintainer changed to Tory Eisenlohr-Moul.
* Documentation fixes: the `cycledata` help page now documents the `daterated`
  column (it previously said `date`, which the dataset does not contain), and
  `cycle_plot()`'s `align_val` argument is now documented under its correct name
  (was `alignval`).
* Build/packaging hygiene: the pkgdown site (`docs/`) and shinyapps deployment
  records (`rsconnect/`, `inst/shiny/rsconnect/`) are no longer bundled into the
  package tarball; removed a stray `R/.Rapp.history`; tidied a vignette chunk
  label that produced a non-portable figure filename. No effect on installed
  functionality.
* Internal: namespace-qualified `stats::sd()`/`stats::ave()`, imported `rlang`'s
  `:=`, and registered remaining non-standard-evaluation column names, clearing
  the "no visible binding for global variable" check notes. No user-facing change.

# menstrualcycleR 0.1.4

* Documentation/metadata only — no code changes. Removed a dead OSF preprint
  link from the `URL:` field of `DESCRIPTION` and added a "How to cite" section
  to the README pointing to the published paper (Nagpal et al., 2025,
  *Psychoneuroendocrinology*). The canonical citation remains available via
  `citation("menstrualcycleR")` (see `inst/CITATION`).

# menstrualcycleR 0.1.3

* Fixed a bug in `pacts_scaling()` where the **user's original date column was
  left `NA` on fabricated calendar rows**. To compute cycle lengths,
  `pacts_scaling()` internally densifies each participant's calendar so that
  every missing day between their first and last observation becomes a row.
  Those fabricated rows received the package's internal canonical `date`, but
  the column you passed in (e.g. `daterated`) was left `NA` on them. When a
  cycle's ovulation had to be **imputed** (no biomarker-confirmed ovulation),
  the imputed-ovulation day — along with its `cyclic_time*` / `scaled_cycleday*`
  values — could land on exactly such a fabricated row. A downstream analysis
  that joined the PACTS output back to other data **by the original date column
  name** would then silently drop those rows, quietly losing imputed-ovulation
  and scaled cycle-time observations. `pacts_scaling()` now returns a fully
  populated original date column: on fabricated rows it is filled from the
  internal calendar date, and observed dates are never overwritten. Character-
  and factor-typed date columns (a supported input) are coerced to `Date` the
  same way the rest of the pipeline already coerces them; `Date` and `POSIXct`
  date columns keep their type. A regression test asserts that no row carries a
  real `date` or a scaled/cycle value while the original date column is `NA`,
  across `Date`, character, factor, and `POSIXct` date inputs.

* Fixed the same class of bug for the **user's original ID column**. The internal
  densify keeps the package's canonical `id` populated on fabricated calendar rows
  (it is the grouping key), but an original id column passed under a different name
  (e.g. `record_id`, `subject`) was left `NA` on those rows — so a downstream join
  on **id + date** could still drop imputed-ovulation / scaled rows even after the
  date fix. `pacts_scaling()` now also refills the original id column from the
  canonical `id` on fabricated rows (NA-fill only; observed ids are never changed
  and the column's type is preserved). This was latent in typical use because the
  package's own examples name the id column `id`; it surfaces for datasets that
  use any other id column name. A regression test covers character and integer id
  columns under a non-`id` name.


* Fixed a bug in `pacts_scaling()` where the **last participant in a dataset** could receive `NA` for the scaled cycle-time variables across an entire phase (most visibly the luteal phase, i.e. `scaled_cycleday` after `ovtoday == 1`). The internal phase-length loops (`lutmax`, `folmax`, `folmax_impute` in `helper.R`) detected the end of a phase by looking one row ahead and stopped at `nrow(data) - 1`, so the final row of the dataset was never treated as the end of a run. The result was position-dependent: reordering participants so that another came last moved the problem to that participant. The loops now treat the final dataset row as a valid run-end, and a regression test asserts that a participant's scaled output is identical regardless of their position in the dataset. Thanks to Elisabeth Conrad (Freie Universität Berlin) for the clear bug report and reproduction. ([reported via PACTS user correspondence, June 2026])



* Packaging hygiene: declared `purrr` in `Imports` (used in `helper.R` but previously
  undeclared), and added `tidyverse` and `marginaleffects` to `Suggests` so the overview
  vignette builds in a clean environment. Continuous integration (GitHub Actions
  `R-CMD-check`) now runs the full `R CMD check` — including the vignette — on every push
  and pull request.

* `pacts_scaling()` and `cycle_plot_individual()` no longer require `dplyr`/`tidyverse` to be attached. Several internal calls to dplyr verbs (`ungroup()`, `filter()`, `case_when()`, `first()`) and to `rlang::sym()` were not namespace-qualified, so a bare `library(menstrualcycleR)` produced `Error: could not find function "ungroup"`. All such calls are now qualified (`dplyr::`/`rlang::`), and every exported function works with the package loaded on its own.

# menstrualcycleR 0.1.0

First release of **menstrualcycleR**, the companion R package to:

> Nagpal, A., Schmalenberger, K. M., Barone, J. C., Mulligan, E., Stumper, A., Knol, L., Failenschmid, J., Kiesner, J., Peters, J. R., & Eisenlohr-Moul, T. A. (2025). Studying the Menstrual Cycle as a Continuous Variable: Implementing Phase-Aligned Cycle Time Scaling (PACTS) with the menstrualcycleR package. *Psychoneuroendocrinology*, 107584. <https://doi.org/10.1016/j.psyneuen.2025.107584>

## Core functionality

* `pacts_scaling()` computes continuous, phase-aligned cycle-time variables — the recommended `cyclic_time*` measures (which map -1 and +1 to the same hormonal point) plus the `scaled_cycleday*` measures — centered on menses onset or ovulation, with optional ovulation imputation.
* `cycle_plot()` and `cycle_plot_individual()` visualize outcomes across the standardized cycle at the group and individual level, with rolling-average smoothing.
* `cycledata_check()` and `summary_ovulation()` summarize data availability and confirmed-versus-imputed ovulation.
* `launch_app()` opens an interactive Shiny app for cycle scaling, data checks, visualization, and C-PASS (PMDD/MRMD/PME) diagnosis.
* `cycledata` provides an example daily-diary dataset.
* Vignette *Getting Started with menstrualcycleR and Phase-Aligned Cycle Time Scaling (PACTS)* walks through the full workflow, including GAMM modeling with `mgcv` cyclic cubic regression splines.
