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

# Analyze symptom data availability
data_available_info <- cycledata_check(
  data_with_scaling, 
  symptom_columns = c("symptom")
)
#> Warning: ID number 8 has < 10 observations for symptom

# View results
print(data_available_info$by_id)
#> # A tibble: 25 × 4
#>       id symptom_nonNA symptom_luteal symptom_follicular
#>    <int>         <int>          <int>              <int>
#>  1     1            20             10                 10
#>  2     2            22             12                 10
#>  3     3            26             13                 13
#>  4     4            26             11                 15
#>  5     5            18              6                 12
#>  6     6            31             12                 19
#>  7     7            20              6                 14
#>  8     8             9              0                  9
#>  9     9            29             13                 16
#> 10    10            28             11                 17
#> # ℹ 15 more rows
print(data_available_info$overall)
#> # A tibble: 1 × 3
#>   symptom_nonNA symptom_luteal symptom_follicular
#>           <int>          <int>              <int>
#> 1           575            218                357
print(data_available_info$data_symptom_plots$symptom)
```
