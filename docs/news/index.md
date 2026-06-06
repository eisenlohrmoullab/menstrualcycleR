# Changelog

## menstrualcycleR 0.1.0

First release of **menstrualcycleR**, the companion R package to:

> Nagpal, A., Schmalenberger, K. M., Barone, J. C., Mulligan, E.,
> Stumper, A., Knol, L., Failenschmid, J., Kiesner, J., Peters, J. R., &
> Eisenlohr-Moul, T. A. (2025). Studying the Menstrual Cycle as a
> Continuous Variable: Implementing Phase-Aligned Cycle Time Scaling
> (PACTS) with the menstrualcycleR package. *Psychoneuroendocrinology*,
> 107584.
> <https://www.sciencedirect.com/science/article/pii/S0306453025003075>

### Core functionality

- [`pacts_scaling()`](../reference/pacts_scaling.md) computes
  continuous, phase-aligned cycle-time variables — the recommended
  `cyclic_time*` measures (which map -1 and +1 to the same hormonal
  point) plus the `scaled_cycleday*` measures — centered on menses onset
  or ovulation, with optional ovulation imputation.
- [`cycle_plot()`](../reference/cycle_plot.md) and
  [`cycle_plot_individual()`](../reference/cycle_plot_individual.md)
  visualize outcomes across the standardized cycle at the group and
  individual level, with rolling-average smoothing.
- [`cycledata_check()`](../reference/cycledata_check.md) and
  [`summary_ovulation()`](../reference/summary_ovulation.md) summarize
  data availability and confirmed-versus-imputed ovulation.
- [`launch_app()`](../reference/launch_app.md) opens an interactive
  Shiny app for cycle scaling, data checks, visualization, and C-PASS
  (PMDD/MRMD/PME) diagnosis.
- `cycledata` provides an example daily-diary dataset.
- Vignette *Getting Started with menstrualcycleR and Phase-Aligned Cycle
  Time Scaling (PACTS)* walks through the full workflow, including GAMM
  modeling with `mgcv` cyclic cubic regression splines.
