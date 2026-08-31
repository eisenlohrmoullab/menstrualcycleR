# Launch the Menstrual Cycle Shiny App

This function launches an interactive Shiny application designed to help
users upload and process their menstrual cycle data. The app provides
tools to apply Phase-Aligned Cycle Time Scaling (PACTS), generate scaled
cycleday variables, and visualize results in an intuitive, user-friendly
interface.

## Usage

``` r
launch_app()
```

## Details

Users can upload a `.csv` file, process their data using built-in PACTS
functionality, and explore cycle-aligned visualizations to support
analysis and interpretation.

Requires the shinyjs and cpass packages, which are Suggests (not
installed automatically with menstrualcycleR) because they are only
needed for this app, not for
[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
or any other exported function. Install them with
`install.packages("shinyjs")` and
`remotes::install_github("lasy/cpass")`.
