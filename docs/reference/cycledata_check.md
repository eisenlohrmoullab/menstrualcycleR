# Analyze Non-Missing Symptom Data by Cycle Phase

This function provides detailed information about the availability of
non-missing symptom data, stratified by cycle phases (luteal and
follicular) and overall availability. It also generates visualizations
to aid in data quality assessment.

## Usage

``` r
cycledata_check(data, symptom_columns)
```

## Arguments

- data:

  A data frame containing the input data.

  - Must include columns for scaled cycle day variables and the symptom
    or dependent variables of interest.

- symptom_columns:

  A character vector of column names representing the dependent
  variables of interest. For example,
  `c("symptom1", "symptom2", "symptom3")`, where each name corresponds
  to a column in your data set.

## Value

A list containing three components:

- **`by_id`**: A data frame summarizing non-missing values for each ID
  and symptom, including:

  1.  Total number of non-missing values.

  2.  Number of non-missing values during the luteal phase.

  3.  Number of non-missing values during the follicular phase.

- **`overall`**: A data frame summarizing non-missing values for each
  symptom, including:

  1.  Total number of non-missing values.

  2.  Number of non-missing values during the luteal phase.

  3.  Number of non-missing values during the follicular phase.

- **Plots**: Visualizations showing the availability of non-missing data
  for each symptom, stratified by ID.

## Examples

``` r
# Load example dataset
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
#> Error in ungroup(.): could not find function "ungroup"

# Analyze symptom data availability
data_available_info <- cycledata_check(
  data_with_scaling, 
  symptom_columns = c("symptom")
)
#> Error: object 'data_with_scaling' not found

# View results
print(data_available_info$by_id)
#> Error: object 'data_available_info' not found
print(data_available_info$overall)
#> Error: object 'data_available_info' not found
print(data_available_info$data_symptom_plots$symptom)
#> Error: object 'data_available_info' not found
```
