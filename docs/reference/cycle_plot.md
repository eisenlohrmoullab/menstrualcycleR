# Plot Symptom Data Across the Menstrual Cycle

This function creates a visual representation of symptom data across the
menstrual cycle. It calculates mean and deviation values for the symptom
by participant, allows centering the visualization on menses or
ovulation, and provides flexibility in choosing the y-axis scale for the
plot.

## Usage

``` r
cycle_plot(
  data,
  symptom,
  centering = "menses",
  include_impute = TRUE,
  y_scale = "person-centered_roll",
  rollingavg = 5,
  align_val = "center",
  se = F
)
```

## Arguments

- data:

  A dataframe containing the input data. Must include columns for scaled
  cycle day variables and the symptom variable. Scaled cycle day
  variables are added to your dataframe after applying the
  calculate_mcyclength() and calculate_cycletime() functions. (See
  examples below)

- symptom:

  A string specifying the symptom variable to analyze.

- centering:

  A string indicating the centering phase of the cycle ("menses" or
  "ovulation"). Default is "menses".

- include_impute:

  A boolean indicating whether to use imputed cycle time values based on
  NC cycle length norms. Default is TRUE.

- y_scale:

  A string specifying the y-axis scale. Options are "person-centered",
  "person-centered_roll", or "means". Default is "person-centered_roll".

- rollingavg:

  A numeric indicating how many days of a rolling average to use, the
  default is 5

- align_val:

  From the zoo package, using rollapply: alignment of rolling avg. Can
  be "center", "left", "right"

- se:

  A boolean indicating whether to include a standard error ribbon.
  Default is False

## Value

A list containing the following elements: 1. A dataframe: The input
dataframe augmented with person-centered mean (.m) and deviation (.d)
values for the symptom variable. 2. A summary dataframe: Contains the
summarized y-axis variable values for each 5% increment of the cycle
percentage. 3. A ggplot object: A plot visualizing the summarized data
across the menstrual cycle.

## Examples

``` r
# Example usage:
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

cycle_plot_data <- cycle_plot(
  data_with_scaling,
  "symptom",
  centering = "menses",
  include_impute = TRUE,
  y_scale = "person-centered"
)
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the menstrualcycleR package.
#>   Please report the issue at
#>   <https://github.com/eisenlohrmoullab/menstrualcycleR/issues>.
cycle_plot_data$plot
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_line()`).
```
