# Changelog

## menstrualcycleR 0.1.9

Adds `mcyclength_complete`, returned alongside `mcyclength` from
[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md).
Situation: on `cycle_incomplete == 1` rows, `mcyclength` is
days-observed-so-far in a still-open trailing cycle, not a cycle length.
Filtering on `mcyclength` alone, without a separate
`cycle_incomplete == 0` clause, silently admits those rows whenever the
observed-so-far count happens to land in range. Before: getting this
right required remembering that second clause every time (documented
since 0.1.8, but easy to miss – a downstream analysis filtered on
`mcyclength` alone and admitted 263 person-days from a still-open cycle
before the omission was caught). Now: `mcyclength_complete` is `NA`
whenever `cycle_incomplete == 1`, so
`filter(mcyclength_complete >= 21, mcyclength_complete <= 35)` excludes
those rows automatically, with no second clause needed. `mcyclength`
itself is unchanged.

Also fixes the same defect internally in
[`summary_ovulation()`](https://menstrualcycler.clearlabresearch.com/reference/summary_ovulation.md)’s
`cycles_outside_norm` calculation, which compared `mcyclength` (not
`mcyclength_complete`) to its `[21,35]` reference norm. This value is
currently dropped before
[`summary_ovulation()`](https://menstrualcycler.clearlabresearch.com/reference/summary_ovulation.md)
returns, so no user-visible output changes – the fix is so the
computation is correct if this column is ever exposed later, rather than
a landmine waiting to be uncovered.

One clarification while documenting this, since it is easy to over-read
the 0.1.8 correction as “the length bound never matters”:
`lower_cyclength_bound`/ `upper_cyclength_bound` fully gate cycle-time
computation for a cycle with **no** confirmed ovulation – a closed cycle
outside the bound gets zero `cyclic_time_impute` coverage, not partial,
until the bound is widened to include it. The bound does not gate a
**confirmed**-ovulation cycle, which scales on its phase lengths
regardless of `mcyclength` (see the vignette’s “Cycle Length Inclusion
Criteria” section). The published PACTS paper’s summary of this rule
(Nagpal et al., 2025, *Psychoneuroendocrinology* 181:107584, Section
2.1.1) predates the 0.1.8 correction and does not distinguish the two
cases.

Also corrects several pre-existing documentation errors found while
reviewing the above, none related to `mcyclength_complete`: the
vignette’s “Interpreting the GAMM Summary Output” section had a pasted
model-fit snapshot (n=575, R-sq.(adj)=0.533) that had drifted since the
0.1.7 phase-cap fallback and no longer matched a live run of the
vignette’s own code (n=611, R-sq.(adj)=0.528) – every number in that
section is now current. A “574” in the data-availability section
contradicted “575” from the live chunk directly above it. Three
citations
([`?pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md),
[`?summary_ovulation`](https://menstrualcycler.clearlabresearch.com/reference/summary_ovulation.md),
the vignette’s reference list) pointed at the retired OSF preprint
instead of the published paper.
[`process_luteal_phase_base()`](https://menstrualcycler.clearlabresearch.com/reference/process_luteal_phase_base.md)
was missing two `@param` entries. A couple of smaller
copy-paste/staleness fixes round it out – see the git history for the
full list.

## menstrualcycleR 0.1.8

Corrects some errors in the vignette and fixes a few small bugs. No
scaled cycle-time values change for any existing user.

To clarify how scaling eligibility works: `lower_cyclength_bound` /
`upper_cyclength_bound` determine which cycles are eligible for an
**imputed** ovulation. A cycle with a **confirmed** ovulation is scaled
based on its phase lengths (luteal 7–18 days, follicular 8–25 days, each
adjustable). The vignette now describes this accurately, and shows how
to filter by cycle length after scaling for users who want that.

Also fixes a crash affecting complete cycles of 14 days or fewer when
eligible for ovulation imputation, plus a few minor internal
corrections.

## menstrualcycleR 0.1.7

Four bug fixes that change scaled cycle-time values in specific
situations, four new arguments, input validation, and documentation
updates. Each fix states the situation it applies to.

### Bug fixes

- **Confirmed ovulation with no later menses in the data.** Situation: a
  participant’s last row is N days after a confirmed ovulation, and no
  menses onset is observed after that ovulation. Before: `cyclic_time`,
  `cyclic_time_ov`, and `luteal_length` treated day N as the last day of
  the luteal phase. Example: with N = 6, day 6 received the value of the
  day before menses and day 3 received the value of mid-luteal. Now:
  those N days are `NA` in all three columns. The same days can be
  scaled with `impute_next_menses = TRUE`, which adds an onset at
  ovulation + 14 days (LH + 15), sets `menses_impute = 1` on that row,
  and scales the days in the imputed columns. This behavior existed in
  every release before 0.1.7.

- **`POSIXct` dates.** Situation: the date column is `POSIXct` and a
  value has a non-midnight time (for example 2024-03-01 09:00). Before:
  that value became `NA` and the row was dropped; if the dropped row was
  an ovulation day, the ovulation was lost; if every row had a
  non-midnight time, the function stopped with an internal error. Now:
  the time is dropped and the date is kept. A value that cannot be read
  as a date produces a warning.

- **`impute_next_menses` with a late closing menses.** Situation:
  `impute_next_menses = TRUE`, a confirmed ovulation, and the next
  observed menses onset more than 20 days after that ovulation. Before:
  the observed onset was not found, and an imputed onset was added at
  ovulation + 14 days even though an observed onset existed. Example: an
  observed 22-day luteal phase received an imputed onset on day 14. Now:
  the search for an observed onset extends to the participant’s next
  confirmed ovulation, including an onset on that ovulation’s date, and
  no onset is imputed when one is found. `next_menses_max_window` no
  longer has an effect and warns if supplied.

- **Luteal phase over 18 days or follicular phase over 25 days, with
  confirmed ovulation.** Situation: a confirmed-ovulation cycle within
  `lower_cyclength_bound` and `upper_cyclength_bound` in which the
  luteal phase exceeds 18 days or the follicular phase exceeds 25 days.
  Before: the days beyond the cap were `NA` in `cyclic_time_impute` and
  `cyclic_time_imp_ov`, as well as in `cyclic_time` and
  `cyclic_time_ov`. Now: `cyclic_time_impute` and `cyclic_time_imp_ov`
  are computed for those days using the observed phase length, provided
  each phase is at least its minimum (7 days luteal, 8 days follicular).
  New columns `cyclic_time_impute_extended_phase` and
  `cyclic_time_imp_ov_extended_phase` equal 1 on the days filled this
  way and 0 otherwise. `cyclic_time` and `cyclic_time_ov` are unchanged.

### New arguments to `pacts_scaling()`

- `luteal_phase_min_days` (7), `luteal_phase_max_days` (18),
  `follicular_phase_min_days` (8), `follicular_phase_max_days` (25).
  These were fixed values before 0.1.7. Defaults give the same output as
  0.1.6. Raising a maximum extends `cyclic_time` and `cyclic_time_ov` to
  the longer phase. Lowering a maximum does not change
  `cyclic_time_impute` or `cyclic_time_imp_ov`, because the fallback in
  the previous bullet does not use the maximum. Details in
  [`?pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md),
  section “Internal phase-length caps”.

- Input validation. Before: a missing column stopped with an rlang
  error; `menses` or `ovtoday` coded as text (`"yes"`/`"no"`) or as
  bleeding intensity (0, 1, 2, 3) returned every derived column as `NA`
  with no message. Now: each case stops with an error that names the
  column and the problem.

### Documentation and packaging

- [`?pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  documents the four new arguments, the phase-length caps, and every
  returned column including `cyclic_time`, `cyclic_time_impute`,
  `cyclic_time_ov`, and `cyclic_time_imp_ov`.
  [`?menstrualcycleR`](https://menstrualcycler.clearlabresearch.com/reference/menstrualcycleR-package.md)
  added. The vignette’s description of `impute_next_menses` updated.
- `R CMD check`: 0 errors, 0 warnings, 0 notes. Debug
  [`print()`](https://rdrr.io/r/base/print.html) output removed from
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md).
  `mgcv`, `shinyjs`, and `cpass` moved from Imports to Suggests.
  Regression tests added for each fix.

## menstrualcycleR 0.1.6

- New opt-in argument `impute_next_menses` in
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  (default `FALSE`). When `TRUE`, a next-menses onset is imputed
  *forward* from a biomarker-confirmed ovulation that has no recorded
  closing menses, placed at `ovulation + next_menses_luteal_days`
  (default 14 days, the population-average luteal length, i.e. the
  “LH+15” / last-follicular-day convention). That cycle then becomes
  scalable instead of being dropped for a missing anchor. Imputed onsets
  are flagged in a new `menses_impute` column so imputed and observed
  onsets stay distinguishable.
- Two companion arguments tune the rule: `next_menses_luteal_days`
  (default 14) and `next_menses_max_window` (default 20; skip imputation
  for an ovulation if an observed menses already closes the cycle within
  that many days).
- This complements the existing ovulation imputation, which imputes
  ovulation *backward* from an observed menses (menses minus 15). The
  package can now close a cycle from either direction when one anchor is
  missing.
- The default (`FALSE`) leaves all previous behavior byte for byte
  identical; only callers who opt in see any change. Only the general
  rule is applied. Study or protocol specific gating (for example
  blocking imputation across treatment phases or documented off-study
  breaks) remains the caller’s responsibility.

## menstrualcycleR 0.1.5

- Maintainer changed to Tory Eisenlohr-Moul.
- Documentation fixes: the `cycledata` help page now documents the
  `daterated` column (it previously said `date`, which the dataset does
  not contain), and
  [`cycle_plot()`](https://menstrualcycler.clearlabresearch.com/reference/cycle_plot.md)’s
  `align_val` argument is now documented under its correct name (was
  `alignval`).
- Build/packaging hygiene: the pkgdown site (`docs/`) and shinyapps
  deployment records (`rsconnect/`, `inst/shiny/rsconnect/`) are no
  longer bundled into the package tarball; removed a stray
  `R/.Rapp.history`; tidied a vignette chunk label that produced a
  non-portable figure filename. No effect on installed functionality.
- Internal: namespace-qualified
  [`stats::sd()`](https://rdrr.io/r/stats/sd.html)/[`stats::ave()`](https://rdrr.io/r/stats/ave.html),
  imported `rlang`’s `:=`, and registered remaining
  non-standard-evaluation column names, clearing the “no visible binding
  for global variable” check notes. No user-facing change.

## menstrualcycleR 0.1.4

- Documentation/metadata only — no code changes. Removed a dead OSF
  preprint link from the `URL:` field of `DESCRIPTION` and added a “How
  to cite” section to the README pointing to the published paper (Nagpal
  et al., 2025, *Psychoneuroendocrinology*). The canonical citation
  remains available via `citation("menstrualcycleR")` (see
  `inst/CITATION`).

## menstrualcycleR 0.1.3

- Fixed a bug in
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  where the **user’s original date column was left `NA` on fabricated
  calendar rows**. To compute cycle lengths,
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  internally densifies each participant’s calendar so that every missing
  day between their first and last observation becomes a row. Those
  fabricated rows received the package’s internal canonical `date`, but
  the column you passed in (e.g. `daterated`) was left `NA` on them.
  When a cycle’s ovulation had to be **imputed** (no biomarker-confirmed
  ovulation), the imputed-ovulation day — along with its `cyclic_time*`
  / `scaled_cycleday*` values — could land on exactly such a fabricated
  row. A downstream analysis that joined the PACTS output back to other
  data **by the original date column name** would then silently drop
  those rows, quietly losing imputed-ovulation and scaled cycle-time
  observations.
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  now returns a fully populated original date column: on fabricated rows
  it is filled from the internal calendar date, and observed dates are
  never overwritten. Character- and factor-typed date columns (a
  supported input) are coerced to `Date` the same way the rest of the
  pipeline already coerces them; `Date` and `POSIXct` date columns keep
  their type. A regression test asserts that no row carries a real
  `date` or a scaled/cycle value while the original date column is `NA`,
  across `Date`, character, factor, and `POSIXct` date inputs.

- Fixed the same class of bug for the **user’s original ID column**. The
  internal densify keeps the package’s canonical `id` populated on
  fabricated calendar rows (it is the grouping key), but an original id
  column passed under a different name (e.g. `record_id`, `subject`) was
  left `NA` on those rows — so a downstream join on **id + date** could
  still drop imputed-ovulation / scaled rows even after the date fix.
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  now also refills the original id column from the canonical `id` on
  fabricated rows (NA-fill only; observed ids are never changed and the
  column’s type is preserved). This was latent in typical use because
  the package’s own examples name the id column `id`; it surfaces for
  datasets that use any other id column name. A regression test covers
  character and integer id columns under a non-`id` name.

- Fixed a bug in
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  where the **last participant in a dataset** could receive `NA` for the
  scaled cycle-time variables across an entire phase (most visibly the
  luteal phase, i.e. `scaled_cycleday` after `ovtoday == 1`). The
  internal phase-length loops (`lutmax`, `folmax`, `folmax_impute` in
  `helper.R`) detected the end of a phase by looking one row ahead and
  stopped at `nrow(data) - 1`, so the final row of the dataset was never
  treated as the end of a run. The result was position-dependent:
  reordering participants so that another came last moved the problem to
  that participant. The loops now treat the final dataset row as a valid
  run-end, and a regression test asserts that a participant’s scaled
  output is identical regardless of their position in the dataset.
  Thanks to Elisabeth Conrad (Freie Universität Berlin) for the clear
  bug report and reproduction. (\[reported via PACTS user
  correspondence, June 2026\])

- Packaging hygiene: declared `purrr` in `Imports` (used in `helper.R`
  but previously undeclared), and added `tidyverse` and
  `marginaleffects` to `Suggests` so the overview vignette builds in a
  clean environment. Continuous integration (GitHub Actions
  `R-CMD-check`) now runs the full `R CMD check` — including the
  vignette — on every push and pull request.

- [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  and
  [`cycle_plot_individual()`](https://menstrualcycler.clearlabresearch.com/reference/cycle_plot_individual.md)
  no longer require `dplyr`/`tidyverse` to be attached. Several internal
  calls to dplyr verbs
  ([`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html),
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  [`case_when()`](https://dplyr.tidyverse.org/reference/case_when.html),
  [`first()`](https://dplyr.tidyverse.org/reference/nth.html)) and to
  [`rlang::sym()`](https://rlang.r-lib.org/reference/sym.html) were not
  namespace-qualified, so a bare
  [`library(menstrualcycleR)`](https://menstrualcycler.clearlabresearch.com)
  produced `Error: could not find function "ungroup"`. All such calls
  are now qualified (`dplyr::`/`rlang::`), and every exported function
  works with the package loaded on its own.

## menstrualcycleR 0.1.0

First release of **menstrualcycleR**, the companion R package to:

> Nagpal, A., Schmalenberger, K. M., Barone, J. C., Mulligan, E.,
> Stumper, A., Knol, L., Failenschmid, J., Kiesner, J., Peters, J. R., &
> Eisenlohr-Moul, T. A. (2025). Studying the Menstrual Cycle as a
> Continuous Variable: Implementing Phase-Aligned Cycle Time Scaling
> (PACTS) with the menstrualcycleR package. *Psychoneuroendocrinology*,
> 107584. <https://doi.org/10.1016/j.psyneuen.2025.107584>

### Core functionality

- [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  computes continuous, phase-aligned cycle-time variables — the
  recommended `cyclic_time*` measures (which map -1 and +1 to the same
  hormonal point) plus the `scaled_cycleday*` measures — centered on
  menses onset or ovulation, with optional ovulation imputation.
- [`cycle_plot()`](https://menstrualcycler.clearlabresearch.com/reference/cycle_plot.md)
  and
  [`cycle_plot_individual()`](https://menstrualcycler.clearlabresearch.com/reference/cycle_plot_individual.md)
  visualize outcomes across the standardized cycle at the group and
  individual level, with rolling-average smoothing.
- [`cycledata_check()`](https://menstrualcycler.clearlabresearch.com/reference/cycledata_check.md)
  and
  [`summary_ovulation()`](https://menstrualcycler.clearlabresearch.com/reference/summary_ovulation.md)
  summarize data availability and confirmed-versus-imputed ovulation.
- [`launch_app()`](https://menstrualcycler.clearlabresearch.com/reference/launch_app.md)
  opens an interactive Shiny app for cycle scaling, data checks,
  visualization, and C-PASS (PMDD/MRMD/PME) diagnosis.
- `cycledata` provides an example daily-diary dataset.
- Vignette *Getting Started with menstrualcycleR and Phase-Aligned Cycle
  Time Scaling (PACTS)* walks through the full workflow, including GAMM
  modeling with `mgcv` cyclic cubic regression splines.
