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


* Fixed a bug in `pacts_scaling()` where the **last participant in a dataset** could receive `NA` for the scaled cycle-time variables across an entire phase (most visibly the luteal phase, i.e. `scaled_cycleday` after `ovtoday == 1`). The internal phase-length loops (`lutmax`, `folmax`, `folmax_impute` in `helper.R`) detected the end of a phase by looking one row ahead and stopped at `nrow(data) - 1`, so the final row of the dataset was never treated as the end of a run. The result was position-dependent: reordering participants so that another came last moved the problem to that participant. The loops now treat the final dataset row as a valid run-end, and a regression test asserts that a participant's scaled output is identical regardless of their position in the dataset. Thanks to Elisabeth Conrad (Freie Universität Berlin) for the clear bug report and reproduction. ([reported via PACTS user correspondence, June 2026])



* `pacts_scaling()` and `cycle_plot_individual()` no longer require `dplyr`/`tidyverse` to be attached. Several internal calls to dplyr verbs (`ungroup()`, `filter()`, `case_when()`, `first()`) and to `rlang::sym()` were not namespace-qualified, so a bare `library(menstrualcycleR)` produced `Error: could not find function "ungroup"`. All such calls are now qualified (`dplyr::`/`rlang::`), and every exported function works with the package loaded on its own.

# menstrualcycleR 0.1.0

First release of **menstrualcycleR**, the companion R package to:

> Nagpal, A., Schmalenberger, K. M., Barone, J. C., Mulligan, E., Stumper, A., Knol, L., Failenschmid, J., Kiesner, J., Peters, J. R., & Eisenlohr-Moul, T. A. (2025). Studying the Menstrual Cycle as a Continuous Variable: Implementing Phase-Aligned Cycle Time Scaling (PACTS) with the menstrualcycleR package. *Psychoneuroendocrinology*, 107584. <https://www.sciencedirect.com/science/article/pii/S0306453025003075>

## Core functionality

* `pacts_scaling()` computes continuous, phase-aligned cycle-time variables — the recommended `cyclic_time*` measures (which map -1 and +1 to the same hormonal point) plus the `scaled_cycleday*` measures — centered on menses onset or ovulation, with optional ovulation imputation.
* `cycle_plot()` and `cycle_plot_individual()` visualize outcomes across the standardized cycle at the group and individual level, with rolling-average smoothing.
* `cycledata_check()` and `summary_ovulation()` summarize data availability and confirmed-versus-imputed ovulation.
* `launch_app()` opens an interactive Shiny app for cycle scaling, data checks, visualization, and C-PASS (PMDD/MRMD/PME) diagnosis.
* `cycledata` provides an example daily-diary dataset.
* Vignette *Getting Started with menstrualcycleR and Phase-Aligned Cycle Time Scaling (PACTS)* walks through the full workflow, including GAMM modeling with `mgcv` cyclic cubic regression splines.
