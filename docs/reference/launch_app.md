# Launch the Menstrual Cycle Shiny App

This function launches an interactive Shiny application designed to help
users upload and process their menstrual cycle data. The app provides
tools to apply Phase-Aligned Cycle Time Scaling (PACTS), generate scaled
cycleday variables, and visualize results in a browser interface.

## Usage

``` r
launch_app()
```

## Value

Called for its side effect of launching the Shiny application. Returns
the result of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)
invisibly; the call blocks until the app is closed. Throws an error,
without launching, if any required package is missing or if the app
directory cannot be found.

## Details

Users can upload a `.csv` file, process their data using built-in PACTS
functionality, and explore cycle-aligned visualizations to support
analysis and interpretation.

Requires three packages that are not installed automatically with
menstrualcycleR, because they are needed only for this app and not for
[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
or any other exported function. shinyjs and writexl are suggested
dependencies and install from CRAN; writexl backs the app's download
buttons. cpass is not on CRAN and installs from GitHub with
`remotes::install_github("lasy/cpass")`; it powers the app's optional
CPASS tab only. `launch_app()` checks for both and reports which are
missing rather than failing part-way through.
