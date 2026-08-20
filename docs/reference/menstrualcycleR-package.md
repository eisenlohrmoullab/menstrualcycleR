# menstrualcycleR: Phase-Aligned Cycle Time Scaling (PACTS)

Tools for standardizing menstrual cycle data onto a continuous,
phase-aligned timeline (PACTS), so cycle-time predictors can be modeled
the same way across participants and cycles despite variability in cycle
length and ovulation timing.

## Where to start

[`pacts_scaling`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/pacts_scaling.md)
is the main entry point – it takes a long-format diary (one row per
person-day) with menses and ovulation markers and returns the same data
with `cyclic_time`, `cyclic_time_impute`, `cyclic_time_ov`, and
`cyclic_time_imp_ov` added (see
[`?pacts_scaling`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/pacts_scaling.md)'s
`@return` for what each means and which to use). For a full worked
example, including GAMM modeling of the resulting cycle-time variables,
see the "Getting Started" vignette:
[`vignette("menstrualcycleR-overview", package = "menstrualcycleR")`](https://eisenlohrmoullab.github.io/menstrualcycleR/articles/menstrualcycleR-overview.md),
or the hosted copy at
<https://eisenlohrmoullab.github.io/menstrualcycleR/articles/menstrualcycleR-overview.html>
– installing via
[`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html)
does not build vignettes locally unless you pass
`build_vignettes = TRUE`.

For a visual explainer of *why* PACTS realigns cycles the way it does,
see
<https://eisenlohrmoullab.github.io/menstrualcycleR/pacts-explainer.html>.

## Other exported functions

- [`cycledata_check`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/cycledata_check.md):
  summarize per-symptom, per-phase data availability before scaling.

- [`summary_ovulation`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/summary_ovulation.md):
  summarize confirmed-vs-imputed ovulation rates after scaling.

- [`cycle_plot`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/cycle_plot.md)
  /
  [`cycle_plot_individual`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/cycle_plot_individual.md):
  visualize a symptom across phase-aligned cycle time, across the whole
  sample or for one individual.

- [`launch_app`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/launch_app.md):
  an interactive Shiny app covering the same workflow (requires the
  shinyjs and cpass packages, installed separately – see
  [`?launch_app`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/launch_app.md)).

## References

Nagpal, A., Schmalenberger, K. M., Barone, J. C., Mulligan, E., Stumper,
A., Knol, L., Failenschmid, J., Kiesner, J., Peters, J. R., &
Eisenlohr-Moul, T. A. (2025). Studying the Menstrual Cycle as a
Continuous Variable: Implementing Phase-Aligned Cycle Time Scaling
(PACTS) with the `menstrualcycleR` package. *Psychoneuroendocrinology*,
107584.
[doi:10.1016/j.psyneuen.2025.107584](https://doi.org/10.1016/j.psyneuen.2025.107584)

## See also

Useful links:

- <https://eisenlohrmoullab.github.io/menstrualcycleR/>

- <https://github.com/eisenlohrmoullab/menstrualcycleR>

- Report bugs at
  <https://github.com/eisenlohrmoullab/menstrualcycleR/issues>

## Author

**Maintainer**: Tory Eisenlohr-Moul <temo@uchicago.edu>
([ORCID](https://orcid.org/0000-0002-9231-3105))

Authors:

- Anisha Nagpal <anagpa7@uic.edu>
  ([ORCID](https://orcid.org/0000-0001-7111-9322))
