# Changelog

## menstrualcycleR 0.1.5

- Maintainer changed to Tory Eisenlohr-Moul.
- Documentation fixes: the `cycledata` help page now documents the
  `daterated` column (it previously said `date`, which the dataset does
  not contain), and
  [`cycle_plot()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/cycle_plot.md)’s
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
  [`pacts_scaling()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/pacts_scaling.md)
  where the **user’s original date column was left `NA` on fabricated
  calendar rows**. To compute cycle lengths,
  [`pacts_scaling()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/pacts_scaling.md)
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
  [`pacts_scaling()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/pacts_scaling.md)
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
  [`pacts_scaling()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/pacts_scaling.md)
  now also refills the original id column from the canonical `id` on
  fabricated rows (NA-fill only; observed ids are never changed and the
  column’s type is preserved). This was latent in typical use because
  the package’s own examples name the id column `id`; it surfaces for
  datasets that use any other id column name. A regression test covers
  character and integer id columns under a non-`id` name.

- Fixed a bug in
  [`pacts_scaling()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/pacts_scaling.md)
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

- [`pacts_scaling()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/pacts_scaling.md)
  and
  [`cycle_plot_individual()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/cycle_plot_individual.md)
  no longer require `dplyr`/`tidyverse` to be attached. Several internal
  calls to dplyr verbs
  ([`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html),
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  [`case_when()`](https://dplyr.tidyverse.org/reference/case_when.html),
  [`first()`](https://dplyr.tidyverse.org/reference/nth.html)) and to
  [`rlang::sym()`](https://rlang.r-lib.org/reference/sym.html) were not
  namespace-qualified, so a bare
  [`library(menstrualcycleR)`](https://eisenlohrmoullab.github.io/menstrualcycleR/)
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

- [`pacts_scaling()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/pacts_scaling.md)
  computes continuous, phase-aligned cycle-time variables — the
  recommended `cyclic_time*` measures (which map -1 and +1 to the same
  hormonal point) plus the `scaled_cycleday*` measures — centered on
  menses onset or ovulation, with optional ovulation imputation.
- [`cycle_plot()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/cycle_plot.md)
  and
  [`cycle_plot_individual()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/cycle_plot_individual.md)
  visualize outcomes across the standardized cycle at the group and
  individual level, with rolling-average smoothing.
- [`cycledata_check()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/cycledata_check.md)
  and
  [`summary_ovulation()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/summary_ovulation.md)
  summarize data availability and confirmed-versus-imputed ovulation.
- [`launch_app()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/launch_app.md)
  opens an interactive Shiny app for cycle scaling, data checks,
  visualization, and C-PASS (PMDD/MRMD/PME) diagnosis.
- `cycledata` provides an example daily-diary dataset.
- Vignette *Getting Started with menstrualcycleR and Phase-Aligned Cycle
  Time Scaling (PACTS)* walks through the full workflow, including GAMM
  modeling with `mgcv` cyclic cubic regression splines.
