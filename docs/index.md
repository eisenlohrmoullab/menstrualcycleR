# How to Install and Load `menstrualcycleR`

To install and load the `menstrualcycleR` package, follow these steps:

1.  Install the `remotes` package (if not already installed):
    `install.packages("remotes")`

2.  Install `menstrualcycleR` from GitHub

`remotes::install_github("eisenlohrmoullab/menstrualcycleR", build_vignettes = TRUE)`

3.  Load the package

[`library(menstrualcycleR)`](https://menstrualcycler.clearlabresearch.com/)

## Quick start

[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
is the main function: give it a long-format diary (one row per
person-day) with menses and ovulation markers, and it returns the same
data with continuous, phase-aligned cycle-time columns added.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`menstrualcycleR`](https://menstrualcycler.clearlabresearch.com/)`)`\
\
`# cycledata is a small example dataset bundled with the package`\
`scaled`` ``<-`` `[`pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)`(`\
`  ``cycledata``,`\
`  id      ``=`` ``id``,`\
`  date    ``=`` ``daterated``,`\
`  menses  ``=`` ``menses``,`\
`  ovtoday ``=`` ``ovtoday`\
`)`\
\
`# cyclic_time / cyclic_time_impute / cyclic_time_ov / cyclic_time_imp_ov are the`\
`# columns to model -- see ?pacts_scaling for what each one covers`\
[`head`](https://rdrr.io/r/utils/head.html)`(``scaled``[``, `[`c`](https://rdrr.io/r/base/c.html)`(``"id"``, ``"daterated"``, ``"cyclic_time"``, ``"cyclic_time_impute"``)``]``)`

One default worth knowing before you interpret results. The 21–35 day
`lower_cyclength_bound` / `upper_cyclength_bound` gate **ovulation
imputation**, not which cycles get scaled: a cycle with a confirmed
ovulation is scaled whatever its length, gated by its phase lengths
instead. This differs from how Nagpal et al. (2025) Section 2.1.1
describes it.
[`summary_ovulation()`](https://menstrualcycler.clearlabresearch.com/reference/summary_ovulation.md)
reports how many of your cycles fall outside 21–35 days, and
[`?pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
explains the behavior in full.

For the full workflow, including GAMM modeling of the resulting
cycle-time variables, see the vignette
([`vignette("menstrualcycleR-overview")`](https://menstrualcycler.clearlabresearch.com/articles/menstrualcycleR-overview.md)
once installed with `build_vignettes = TRUE` above, or the hosted copy
linked below) or
[`?menstrualcycleR`](https://menstrualcycler.clearlabresearch.com/reference/menstrualcycleR-package.md)
for an overview of every exported function.

To utilize the shinyapp, visit:
<https://menstrualcycledata.shinyapps.io/shiny/>

For a tutorial on using `menstrualcycleR` visit:
<https://menstrualcycler.clearlabresearch.com/articles/menstrualcycleR-overview.html>

For a visual explainer of how PACTS works — why cycle-day counting
misaligns hormones and how PACTS realigns them — visit:
<https://menstrualcycler.clearlabresearch.com/pacts-explainer.html>

To browse an auto-generated, annotated bibliography of papers that cite,
apply, or extend `menstrualcycleR` and PACTS, visit:
<https://menstrualcycler.base44.app>

For a history of changes by version, see the changelog:
<https://menstrualcycler.clearlabresearch.com/news/index.html>

## How to cite

If you use `menstrualcycleR` in your research, please cite:

> Nagpal, A., Schmalenberger, K. M., Barone, J. C., Mulligan, E.,
> Stumper, A., Knol, L., Failenschmid, J., Kiesner, J., Peters, J. R., &
> Eisenlohr-Moul, T. A. (2025). Studying the menstrual cycle as a
> continuous variable: Implementing Phase-Aligned Cycle Time Scaling
> (PACTS) with the `menstrualcycleR` package.
> *Psychoneuroendocrinology*, 107584.
> <https://doi.org/10.1016/j.psyneuen.2025.107584>

You can also run `citation("menstrualcycleR")` in R to get the citation
in plain-text and BibTeX form.

## Use of AI coding tools

`menstrualcycleR` was created by Anisha Nagpal and Tory Eisenlohr-Moul,
and the PACTS method is theirs and their co-authors’. **No AI coding
tool was used in the package’s initial development**, which began in
January 2025 and produced the scaling engine, the exported functions,
and the original documentation.

**Anisha Nagpal’s contributions predate all AI tool use.** Her final
commit is dated 18 March 2026. The first AI-assisted commit is dated 30
May 2026.

The decision to use AI coding tools was made by **Dr. Tory
Eisenlohr-Moul**, the package maintainer, and it applies only to work
after that date: debugging, documentation, and release preparation. Two
tools appear in the history, each marked in the commits it touched —
GitHub Copilot’s coding agent, which updated the citation files in May
2026, and Claude Code, used from June 2026 onward and carrying a
`Co-Authored-By` trailer.

Neither tool is an author, and neither is listed in `Authors@R`.
Dr. Eisenlohr-Moul reviewed and approved every AI-assisted change and is
responsible for the code and for every claim in the documentation.

![](https://github.com/user-attachments/assets/0502430c-75d9-4fdb-9b59-f3bafd16bb9c)
