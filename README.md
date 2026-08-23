# How to Install and Load `menstrualcycleR`

<!-- badges: start -->
[![R-CMD-check](https://github.com/eisenlohrmoullab/menstrualcycleR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/eisenlohrmoullab/menstrualcycleR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

To install and load the `menstrualcycleR` package, follow these steps:

1. Install the `remotes` package (if not already installed):
   `install.packages("remotes")`
   

2. Install `menstrualcycleR` from GitHub 

`remotes::install_github("eisenlohrmoullab/menstrualcycleR", build_vignettes = TRUE)`

3. Load the package 

`library(menstrualcycleR)`

## Quick start

`pacts_scaling()` is the main function: give it a long-format diary (one row per person-day)
with menses and ovulation markers, and it returns the same data with continuous,
phase-aligned cycle-time columns added.

```r
library(menstrualcycleR)

# cycledata is a small example dataset bundled with the package
scaled <- pacts_scaling(
  cycledata,
  id      = id,
  date    = daterated,
  menses  = menses,
  ovtoday = ovtoday
)

# cyclic_time / cyclic_time_impute / cyclic_time_ov / cyclic_time_imp_ov are the
# columns to model -- see ?pacts_scaling for what each one covers
head(scaled[, c("id", "daterated", "cyclic_time", "cyclic_time_impute")])
```

For the full workflow, including GAMM modeling of the resulting cycle-time
variables, see the vignette (`vignette("menstrualcycleR-overview")` once
installed with `build_vignettes = TRUE` above, or the hosted copy linked
below) or `?menstrualcycleR` for an overview of every exported function.

To utilize the shinyapp, visit: https://menstrualcycledata.shinyapps.io/shiny/

For a tutorial on using `menstrualcycleR` visit: https://menstrualcycler.clearlabresearch.com/articles/menstrualcycleR-overview.html

For a visual explainer of how PACTS works — why cycle-day counting misaligns hormones and how PACTS realigns them — visit: https://menstrualcycler.clearlabresearch.com/pacts-explainer.html

To browse an auto-generated, annotated bibliography of papers that cite, apply, or extend `menstrualcycleR` and PACTS, visit: https://menstrualcycler.base44.app

For a history of changes by version, see the changelog: https://menstrualcycler.clearlabresearch.com/news/index.html

## How to cite

If you use `menstrualcycleR` in your research, please cite:

> Nagpal, A., Schmalenberger, K. M., Barone, J. C., Mulligan, E., Stumper, A., Knol, L., Failenschmid, J., Kiesner, J., Peters, J. R., & Eisenlohr-Moul, T. A. (2025). Studying the menstrual cycle as a continuous variable: Implementing Phase-Aligned Cycle Time Scaling (PACTS) with the `menstrualcycleR` package. *Psychoneuroendocrinology*, 107584. https://doi.org/10.1016/j.psyneuen.2025.107584

You can also run `citation("menstrualcycleR")` in R to get the citation in plain-text and BibTeX form.


<img src="https://github.com/user-attachments/assets/0502430c-75d9-4fdb-9b59-f3bafd16bb9c" width="300">

