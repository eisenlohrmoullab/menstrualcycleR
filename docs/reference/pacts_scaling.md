# Phase-Aligned Cycle Time Scaling (PACTS)

This function uses the Phase-Aligned Cycle Time Scaling (PACTS) method,
as described in Nagpal et al. (2025), to compute continuous,
biologically meaningful cycle time variables in a long-format dataset,
where each row represents a unique observation date for a specific
naturally-cycling individual.

## Usage

``` r
pacts_scaling(
  data,
  id,
  date,
  menses,
  ovtoday,
  lower_cyclength_bound = 21,
  upper_cyclength_bound = 35
)
```

## Arguments

- data:

  A data frame containing the required input variables.

- id:

  A unique identifier for each naturally-cycling individual in the
  dataset.

- date:

  A date column corresponding to the day of observation. Ensure all
  id-date combinations are unique. You may wish to reclassify
  post-midnight survey entries to the previous day to maintain alignment
  with sleep or daily tracking data.

- menses:

  A binary column (`0`/`1`) indicating the *first* day of menses onset,
  where `1` marks onset. All subsequent bleeding days should be set to
  `0`. Periovulatory spotting should also be excluded.

- ovtoday:

  A binary column (`0`/`1`) indicating the day of estimated ovulation.
  Required even if biomarkers were not collected—use a column of all
  `0`s or `NA`s in that case. Accepted ovulation determination methods
  include:

  - **Urinary LH surge tests**: Code `ovtoday == 1` on the day *after*
    the first positive test (LH +1). Specify the brand and threshold
    (e.g., 40 mIU/ml).

  - **Basal body temperature (BBT)**: Code `ovtoday == 1` on the day
    after the BBT nadir (BBT +1). Specify measurement method.

  - **Daily hormone assays**: See Nagpal et al. (2025) for guidance.

- lower_cyclength_bound:

  Numeric lower bound of cycle lengths to include in scaling. Default is
  21.

- upper_cyclength_bound:

  Numeric upper bound of cycle lengths to include in scaling. Default is
  35.

## Value

The original data frame with the following additional columns:

- `scaled_cycleday`: A continuous cycle time variable centered on menses
  onset (`menses == 1` → 0), ranging from -1 (start of luteal phase) to
  +1 (ovulation). Only includes cycles with biomarker-confirmed
  ovulation.

- `scaled_cycleday_impute`: Same as above, but includes cycles where
  ovulation was imputed using day -15. Offers broader coverage across
  the dataset, at the cost of lower precision.

- `scaled_cycleday_ov`: A cycle time variable centered on ovulation day
  (`ovtoday == 1` → 0), ranging from -1 (start of follicular phase) to
  +1 (end of luteal phase). Only includes cycles with confirmed
  ovulation.

- `scaled_cycleday_imp_ov`: Same as above, but uses imputed ovulation
  (`ovtoday_impute == 1`) for cycles lacking biomarker confirmation.
  Centered on either confirmed or imputed ovulation.

- `ovtoday_impute`: A binary column indicating imputed ovulation days
  (value `1`) for cycles without confirmed ovulation, estimated as 15
  days before menses onset.

## Details

The PACTS method aligns observations across cycles by centering time
either on menses onset or ovulation. This allows researchers to model
menstrual cycle dynamics as continuous functions of time, improving
sensitivity and interpretability. The function requires identification
of menses onset (`menses`) and the estimated day of ovulation
(`ovtoday`), which may be determined via biomarker (preferred) or
imputed based on typical luteal phase length when unavailable.

When ovulation is not directly assessed, the function imputes ovulation
as 15 days prior to the next menses onset (i.e., the last day of the
follicular phase), based on the population-average luteal phase length.
Imputed ovulation days are recorded in a new binary column,
`ovtoday_impute`.

Reporting how often ovulation was confirmed using biomarkers versus
imputed is important for transparency and scientific rigor. Whenever
possible, researchers should use objective biomarkers such as LH tests
or basal body temperature (BBT) to identify ovulation, as these methods
provide greater precision. This function supports both confirmed and
imputed ovulation, allowing analyses to flexibly account for variable
data availability across participants and cycles.

For further guidance on ovulation identification and justification of
the -15 day imputation approach, see:

- Nagpal et al. (2025). *Studying the Menstrual Cycle as a Continuous
  Variable: Implementing Phase-Aligned Cycle Time Scaling (PACTS) with
  the `menstrualcycleR` package*.
  https://doi.org/10.31219/osf.io/hd5xw_v1

- Schmalenberger et al. (2021). *How to study the menstrual cycle:
  Practical tools and recommendations*. *Psychoneuroendocrinology,
  123*, 104895. https://doi.org/10.1016/j.psyneuen.2020.104895

## Examples

``` r
# Load the example dataset
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


# View the result
print(data_with_scaling)
#> # A tibble: 744 × 20
#>       id date       menses ovtoday symptom daterated  m2mcount mcyclength
#>    <int> <date>      <dbl>   <dbl>   <dbl> <date>        <dbl>      <dbl>
#>  1     1 2024-01-20      1       0       5 2024-01-20        1         24
#>  2     1 2024-01-21      0       0       5 2024-01-21        2         24
#>  3     1 2024-01-22      0       0       3 2024-01-22        3         24
#>  4     1 2024-01-23      0       0      NA NA                4         24
#>  5     1 2024-01-24      0       0       2 2024-01-24        5         24
#>  6     1 2024-01-25      0       0       1 2024-01-25        6         24
#>  7     1 2024-01-26      0       0       1 2024-01-26        7         24
#>  8     1 2024-01-27      0       0       1 2024-01-27        8         24
#>  9     1 2024-01-28      0       0       3 2024-01-28        9         24
#> 10     1 2024-01-29      0       0       4 2024-01-29       10         24
#> # ℹ 734 more rows
#> # ℹ 12 more variables: cycle_incomplete <dbl>, cyclenum <int>,
#> #   ovtoday_impute <int>, scaled_cycleday <dbl>, scaled_cycleday_ov <dbl>,
#> #   scaled_cycleday_impute <dbl>, scaled_cycleday_imp_ov <dbl>,
#> #   cyclic_time <dbl>, cyclic_time_impute <dbl>, cyclic_time_ov <dbl>,
#> #   cyclic_time_imp_ov <dbl>, luteal_length <dbl>
```
