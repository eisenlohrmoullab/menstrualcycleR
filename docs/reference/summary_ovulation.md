# Summarize Ovulation Confirmation and Imputation

This function provides a summary of how ovulation was identified across
the dataset—either through direct confirmation using biomarkers or via
imputation based on menstrual cycle timing.

## Usage

``` r
summary_ovulation(data)
```

## Arguments

- data:

  A dataframe containing the input data. Must include columns for scaled
  cycle day variables and symptoms/dependent variables of interest.
  Scaled cycle day variables are added to your dataframe after applying
  the
  [`pacts_scaling()`](https://eisenlohrmoullab.github.io/menstrualcycleR/reference/pacts_scaling.md)
  function. (See examples below)

## Value

A list with two data frames:

- `ovstatus_total`: A dataset-level summary showing:

  1.  The number of cycles with confirmed ovulation (`ovtoday == 1`).

  2.  The number of cycles with imputed ovulation
      (`ovtoday_impute == 1`).

- `ovstatus_id`: A participant-level summary showing, for each unique
  ID:

  1.  The number of cycles with confirmed ovulation.

  2.  The number of cycles with imputed ovulation.

## Details

Specifically, it counts the number of cycles in which ovulation was:

- **Confirmed** using objective biomarkers (i.e., a `1` in the `ovtoday`
  column), such as urinary LH surge tests or basal body temperature
  (BBT).

- **Imputed** using the backward-count method, which estimates ovulation
  as 15 days before the subsequent menses onset (`ovtoday_impute == 1`),
  based on the typical length of the luteal phase.

The output includes both:

- Overall summary counts across the entire dataset.

- Per-individual summaries to support participant-level quality checks
  and reporting.

This summary is essential for understanding data quality and should be
transparently reported in publications. It helps clarify how often
ovulation timing was determined using biologically grounded methods
versus estimated through assumptions. Whenever possible, biomarker-based
confirmation (e.g., LH or BBT) is preferred, as it provides more precise
and physiologically valid estimates of ovulation timing. This enhances
the accuracy of cycle phase alignment and reduces potential sources of
error in time-sensitive analyses.

When biomarkers are unavailable, imputation via the -15 day
backward-count method offers a more biologically valid estimate than
assuming ovulation occurs at the cycle midpoint. This method accounts
for the relative stability of the luteal phase length and is recommended
over midpoint-based estimates (see Nagpal et al., 2025).

For further guidance on ovulation identification and the rationale for
the -15 day imputation method, see:

- Nagpal et al. (2025). *Studying the Menstrual Cycle as a Continuous
  Variable: Implementing Phase-Aligned Cycle Time Scaling (PACTS) with
  the `menstrualcycleR` package*.
  https://doi.org/10.31219/osf.io/hd5xw_v1

- Schmalenberger et al. (2021). *How to study the menstrual cycle:
  Practical tools and recommendations*. *Psychoneuroendocrinology,
  123*, 104895. https://doi.org/10.1016/j.psyneuen.2020.104895

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

ov_summary = summary_ovulation(data_with_scaling)
print(ov_summary)
#> $ovstatus_total
#>          Total Confirmed Ovulation
#> N cycles                        14
#>          Total Estimated Ovulation via 15day Backward Count
#> N cycles                                                 11
#> 
#> $ovstatus_id
#> # A tibble: 25 × 3
#>       id `Total cycles with confirmed ovulation` Total cycles with imputed ovu…¹
#>    <int>                                   <dbl>                           <dbl>
#>  1     1                                       0                               1
#>  2     2                                       1                               0
#>  3     3                                       1                               0
#>  4     4                                       0                               1
#>  5     5                                       1                               0
#>  6     6                                       0                               1
#>  7     7                                       1                               0
#>  8     8                                       1                               0
#>  9     9                                       0                               1
#> 10    10                                       1                               0
#> # ℹ 15 more rows
#> # ℹ abbreviated name:
#> #   ¹​`Total cycles with imputed ovulation via 15day Backward Count`
#> 
ov_summary$ovstatus_total
#>          Total Confirmed Ovulation
#> N cycles                        14
#>          Total Estimated Ovulation via 15day Backward Count
#> N cycles                                                 11
ov_summary$ovstatus_id
#> # A tibble: 25 × 3
#>       id `Total cycles with confirmed ovulation` Total cycles with imputed ovu…¹
#>    <int>                                   <dbl>                           <dbl>
#>  1     1                                       0                               1
#>  2     2                                       1                               0
#>  3     3                                       1                               0
#>  4     4                                       0                               1
#>  5     5                                       1                               0
#>  6     6                                       0                               1
#>  7     7                                       1                               0
#>  8     8                                       1                               0
#>  9     9                                       0                               1
#> 10    10                                       1                               0
#> # ℹ 15 more rows
#> # ℹ abbreviated name:
#> #   ¹​`Total cycles with imputed ovulation via 15day Backward Count`
```
