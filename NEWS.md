# menstrualcycleR 0.1.2

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
