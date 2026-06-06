# Generate Cycle-Specific Plots and Summary Data for a Given ID

This function creates cycle-specific plots and summary statistics for a
specified individual (`id`), storing both in a named list for easy
access.

## Usage

``` r
cycle_plot_individual(
  data,
  id,
  symptoms,
  centering = "menses",
  y_scale = "person-centered",
  include_impute = TRUE,
  rollingavg = 5
)
```

## Arguments

- data:

  A dataframe containing menstrual cycle data, including `id` and
  `cyclenum` columns.

- id:

  Numeric id number for the specific individual for whom cycle plots
  should be generated.

- symptoms:

  A vector of strings specifying the symptom variable to analyze that
  exist in `data`.

- centering:

  A string indicating the centering phase of the cycle ("menses" or
  "ovulation").

- y_scale:

  A string specifying the y-axis scale ("person-centered",
  "person-centered_roll", "raw", or "roll").

- include_impute:

  A boolean indicating whether to use imputed cycle time values.

- rollingavg:

  A numeric indicating how many days of a rolling average to use, the
  default is 5

## Value

A list where each cycle contains:

- `$plot`: The cycle-specific ggplot object with the ID displayed

- `$summary`: The corresponding summary data

## Examples

``` r
cycle_df = cycledata

data_with_scaling <- pacts_scaling(
  cycle_df, 
  id = id, 
  date = daterated, 
  menses = menses, 
  ovtoday = ovtoday, 
  lower_cyclength_bound = 21, 
  upper_cyclength_bound = 35
)
#> id: id
#> date: date
#> menses: menses
#> ovtoday: ovtoday


```
