# menstrualcycleR 0.1.7

* **Deployment-readiness pass (2026-08-19):** `R CMD check` now runs clean (0 errors, 0
  warnings, 0 notes; was 2 notes). Found and fixed:
  - A private, machine-local operator-notes file (deliberately gitignored, never meant to
    leave this machine) was shipping inside the built package tarball, because
    `.Rbuildignore` excluded one of the two repo-root operator-notes files but not the
    other. Fixed; the local settings directory excluded too as a preventive measure.
  - `pacts_scaling()` had no input validation at all: a missing column threw a raw rlang
    stack trace, and -- more seriously -- a character-coded `menses`/`ovtoday` (e.g.
    `"yes"`/`"no"` instead of `0`/`1`) produced **no error or warning**, silently returning
    every derived column as `NA`. Both now fail immediately with a specific message naming
    the argument and the actual problem (missing column, quoted-string column name, wrong
    type, or an out-of-range value).
  - `pacts_scaling()`'s `@return` documented only the legacy `scaled_cycleday*` columns and
    omitted `cyclic_time`/`cyclic_time_impute`/`cyclic_time_ov`/`cyclic_time_imp_ov` --
    the actual recommended output, and the variables reported in Nagpal et al. (2025) --
    along with `mcyclength`/`m2mcount`/`cyclenum`/`cycle_incomplete`/`luteal_length`. All
    now documented; `@return` also states plainly that `scaled_cycleday*` is a legacy,
    independently-computed implementation, not interchangeable with `cyclic_time*`.
  - `calculate_mcyclength()` (called internally by every `pacts_scaling()` call) had
    leftover debug `print()` statements firing unconditionally on every call. Removed.
  - `mgcv` (used only in the vignette) moved from `Imports` to `Suggests`; `shinyjs`/`cpass`
    (used only by the optional `launch_app()` Shiny app) moved from `Imports` to `Suggests`
    too, with `launch_app()` now checking for them and giving an actionable install message
    if missing, rather than requiring every user of `pacts_scaling()` to install them.
  - No `?menstrualcycleR` package-level help topic existed. Added, with a "where to start"
    pointer to `pacts_scaling()` and the vignette.
  - `cycle_plot_individual()`'s own `@examples` never actually called the function it
    documents; fixed, and `@return`'s described list shape corrected to match the real
    (one level deeper than documented) nesting.
  - Smaller documentation fixes: `cycledata_check()`'s `@return` now names its third list
    element's real accessor (`data_symptom_plots`); a stray example comment in `cycle_plot()`
    was silently dropped from the rendered docs (missing `#'` prefix); dead `print()` calls
    after `return()` in `summary_ovulation()` removed; `README.md` gained a runnable
    quick-start example and now recommends `build_vignettes = TRUE` on install.
  - The CI workflow (`R-CMD-check.yaml`) was set to `error-on: "error"` with a comment
    saying to tighten it to `"warning"` once the check's pre-existing WARNING/NOTE backlog
    (tracked in a file that no longer exists) was cleared. It now is; tightened.

* **New:** the 18-day luteal and 25-day follicular phase-length caps described
  below are now caller-adjustable arguments to `pacts_scaling()` --
  `luteal_phase_min_days` / `luteal_phase_max_days` (default `7` / `18`) and
  `follicular_phase_min_days` / `follicular_phase_max_days` (default `8` /
  `25`) -- independent of `lower_cyclength_bound`/`upper_cyclength_bound`,
  which bound the whole cycle, not either phase. Through 0.1.6 these were
  hardcoded literals, not arguments at all. Defaults reproduce prior behavior
  exactly (verified: `pacts_scaling(...)` with no new arguments passed is
  byte-for-byte identical to passing all four explicitly at their defaults). A
  study population with genuinely longer or shorter luteal/follicular phases
  than the Bull et al. (2019) norms these defaults encode can now widen the
  relevant bound directly and get that coverage in `cyclic_time` itself,
  rather than relying only on the narrower `cyclic_time_impute`-only fallback
  described next (which still applies for whatever runs past even a
  caller-widened phase bound). `luteal_length` respects the same configurable
  bounds. Full detail, including the six-argument interaction, in
  `?pacts_scaling`, section "Internal phase-length caps."
* **Fixed:** `cyclic_time_impute` now has a third fallback tier for a confirmed
  ovulation whose luteal phase (ovulation to next menses) exceeds 18 days, or
  whose follicular phase (menses to ovulation) exceeds 25 days -- fixed internal
  limits, independent of the `lower_cyclength_bound`/`upper_cyclength_bound`
  arguments a caller passes. Previously, those days had NO fallback at all:
  `cyclic_time` correctly excludes them (unchanged), but the imputed-ovulation
  path was also silently NA, because `calculate_ovtoday_impute()` deliberately
  suppresses imputed ovulation whenever a confirmed ovulation already exists in
  the cycle (correct, to avoid double-anchoring) -- leaving the whole capped
  phase with no coverage in *either* column. A cycle can therefore sit entirely
  inside a caller's accepted overall length (for example `[20,43]`) and still
  lose a large stretch of `cyclic_time_impute` to this internal cap alone; a
  cycle whose two phases are each individually in-range under the internal
  limits (luteal <=18, follicular <=25) already sums to 43 days by
  construction (18+25=43) -- coincidentally the CLEAR lab's own chosen upper
  bound -- which is why this was most visible on longer cycles.
  `cyclic_time_impute` now falls back, for exactly the days a capped phase would
  otherwise leave uncovered, to the same phase fraction computed *without* the
  internal cap, still gated on the overall cycle falling within the caller's own
  bounds -- so this never scales a cycle *longer* than what the caller already
  accepts. It does **not** bound how far over the original 18/25-day phase norms
  a single phase can run within that overall length (a caller with a wide
  overall bound, e.g. the CLEAR lab's `[20,43]`, can still see a single phase
  scaled well past the Bull et al. 2019 norms the original cap encoded -- this
  fallback trades phase-length plausibility for coverage, deliberately, and
  that tradeoff is now named in `pacts_scaling()`'s roxygen rather than implied).
  `cyclic_time` itself (the confirmed-only, strict column) is completely
  untouched -- byte for byte identical to 0.1.6 for every existing caller. A new
  column, `cyclic_time_impute_extended_phase` (`0`/`1`), flags exactly which
  rows were filled this way (correctly excluding the ovulation-anchor day
  itself, whose value is always pinned by an unrelated, pre-existing override
  regardless of this fallback), so it can be reported alongside the existing
  imputed-ovulation rate, or excluded by an analyst who wants the narrower
  pre-0.1.7 coverage.
* **Fixed** (found in adversarial review of the above, same day): the new
  fallback originally omitted the `cycle_incomplete != 1` guard that every
  other `mcyclength`-gated column in the package already uses. Without it, a
  still-open trailing cycle (data collection ended before a closing menses was
  ever observed) could get fabricated `cyclic_time_impute` coverage whenever its
  observed-so-far span happened to land inside the caller's bounds by
  coincidence of when the study ended. Now gated identically to every sibling
  column.
* **Correction (2026-08-19, later same day):** an earlier version of this entry
  said the follicular cap "was never documented anywhere outside a code
  comment." That was wrong -- checked without reading the whole repo first.
  Both caps ARE disclosed, together, with the load-bearing "fixed vs.
  adjustable" distinction, in `docs/pacts-explainer.html` (linked from
  `README.md` as "a visual explainer of how PACTS works"; added by
  Tory Eisenlohr-Moul, 2026-07-23, commit `a0b0a39`): *"menstrualcycleR scales
  cycles 21-35 days, with luteal phases 7-18 days and follicular phases 8-25
  days (Bull et al., 2019 norms from >600,000 cycles). The cycle-length bounds
  are user-adjustable (lower_cyclength_bound / upper_cyclength_bound); the
  phase-length windows are fixed in the package."* That is a complete,
  accurate disclosure, in a real, currently-linked page -- just not in the
  peer-reviewed paper, and not in the separate "Getting Started" vignette
  (which discloses only the luteal cap, repeatedly, but never the follicular
  one). Corrected picture, checked exhaustively (main paper text, twice; the
  official published supplement, obtained and read in full -- zero mentions of
  either cap in either; the Getting Started vignette, 40 of 48 pages; the
  visual explainer, in full; README, the repo's internal operator notes, and
  all `man/*.Rd` pages):
  - Peer-reviewed paper (Nagpal et al. 2025) + its official supplement: **silent
    on both caps.** Describes only the single adjustable overall-length gate.
  - "Getting Started" vignette: **discloses the luteal cap only**, repeatedly.
  - `pacts-explainer.html`: **discloses both caps**, with the fixed/adjustable
    distinction spelled out, a year after the caps were introduced into the
    code.
  Also confirmed by full git-history archaeology: the exact 7/18 (luteal) and
  8/25 (follicular) day thresholds have been in the codebase since the
  package's first commit (2025-01-05), but only started gating the PACTS
  `cyclic_time`/`cyclic_lut` output on 2025-07-26 (commit `5cff707`) -- two days
  after `cyclic_time` was first written, replacing what had briefly been an
  adjustable `lower_cyclength_bound`/`upper_cyclength_bound` gate instead. That
  replacement predates every tagged release (v0.1.0 was 2026-06-05), so every
  released version of this package has always had this behavior; the two-day
  window where `cyclic_time` used only the adjustable bound never shipped.
  The internal caps are now also described in `pacts_scaling()`'s roxygen
  ("Internal phase-length caps" section), so they're discoverable from R
  itself (`?pacts_scaling`), not only from a separate HTML page.
* **Fixed:** `cyclic_time_imp_ov` (the ovulation-centered sibling of
  `cyclic_time_impute`) had the identical missing-coverage gap and, until now,
  did not receive the fallback described above. It now gets the same
  three-tier fallback -- confirmed (`cyclic_time_ov`) -> imputed-ovulation
  (`cyclic_time_imp_ov1`) -> uncapped phase math, gated on the caller's
  overall cycle-length bound -- mirrored column-for-column, including the
  companion `cyclic_time_imp_ov_extended_phase` flag and the same
  `cycle_incomplete` and ovulation-anchor exclusions. `cyclic_time_ov` itself
  is completely untouched, same guarantee as `cyclic_time`.
* **Fixed:** the existing (0.1.6) `impute_next_menses` feature could inject a
  phantom menses onset into a cycle whose real closing menses fell just
  outside its default 20-day search window, corrupting already-observed data
  (confirmed on the shipped `cycledata` example, participant id 8: a real
  22-day luteal phase got a fabricated onset at day 14, flipping an actually
  -observed `menses == 0` day to `1`). The candidate-onset search
  (`impute_next_menses_onsets()`) is now unbounded -- it always checks for
  *any* later observed menses before treating an ovulation as unclosed, rather
  than only within `next_menses_max_window` days. `next_menses_max_window` is
  now unused (kept as an argument for signature stability; see
  `?pacts_scaling`). One consequence worth naming: with the bug fixed, the
  shipped `cycledata` example now shows **zero** imputed onsets under default
  settings -- every cycle in that example dataset that previously appeared to
  need imputation turns out to have a real closing menses just beyond the old,
  buggy window. That confirms the bug was the entire source of imputation
  activity on that example, not a partial contributor to it.
* **Fixed** (found in a final pre-push adversarial review, same day): the
  unbounded closing-menses search above checked "does *any* later menses
  exist for this id," not "does a menses close *this* ovulation before the
  id's next confirmed ovulation" -- so for an id with more than one
  ovulation, a later ovulation's real closing menses could be misread as
  closing an earlier one too, wrongly suppressing imputation for an earlier
  cycle that has no real closing menses of its own before the next
  ovulation. `impute_next_menses_onsets()` now additionally bounds the
  search by the id's own next confirmed ovulation (not by a day count --
  that was the original 0.1.6 bug) when there is one.
* **Fixed** (same review): `cyclic_time_imp_ov_extended_phase` did not
  account for the pre-existing menses-anchor override that unconditionally
  pins `cyclic_time_imp_ov` to `1` at `menses == 1` -- so a menses-onset row
  could be flagged as "filled by the uncapped fallback" while actually
  showing the pinned value, which can directly contradict what the fallback
  itself computed there (the analogous pin for `cyclic_time_impute`, `0`,
  happens to coincide with the fallback's own value on a menses-onset row,
  masking the identical gap in that column). Both `_extended_phase` flags
  now correctly read `0` whenever the menses-anchor pin is what actually
  determined the published value, mirroring the existing ovulation-anchor
  exclusion.
* **Hardened** (same review, no demonstrated trigger found): `cyclic_fol_ov_uncapped`
  was missing the `percfol_ov` overlap guard its capped sibling `cyclic_fol_ov`
  carries, which exists to prevent double-counting a luteal/follicular
  boundary day. Added for consistency with the capped path; extensive testing
  (500+ randomized datasets plus targeted boundary constructions) did not
  find a case where the missing guard produced a wrong value, since the
  unconditional ovulation-anchor override currently masks the one row where
  it could matter.
* **Documentation:** the "Getting Started" vignette's description of
  `impute_next_menses`/`next_menses_max_window` still described the pre-0.1.7
  bounded-window search; corrected. `pacts_scaling()`'s roxygen now states
  explicitly that narrowing `luteal_phase_max_days`/`follicular_phase_max_days`
  below their defaults does **not** make `cyclic_time_impute`/`cyclic_time_imp_ov`
  stricter (only widening the bound affects them) -- the uncapped fallback
  ignores the ceiling argument entirely and only respects the floor and the
  overall cycle-length bound. `next_menses_max_window` now emits a warning if
  passed explicitly, rather than silently doing nothing.
* Found via a person-by-person review of CLEAR-1/2/3 cycle data (2026-08-19);
  see `pacts-gam-pipeline/cycle_data_prep/CYCLE_METHODS_DECISIONS.md` for the
  full methods-decision record.

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
