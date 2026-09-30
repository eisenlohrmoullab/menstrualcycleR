# Summarize Ovulation Confirmation and Imputation

This function provides a summary of how ovulation was identified across
the dataset – either through direct confirmation using an ovulation
biomarker or via imputation based on menstrual cycle timing.

## Usage

``` r
summary_ovulation(data)
```

## Arguments

- data:

  A dataframe containing the output of
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  – specifically `id`, `cyclenum`, `ovtoday`, `ovtoday_impute`, and
  `mcyclength_complete`. (See examples below)

## Value

A list with two data frames:

- `ovstatus_total`: A dataset-level summary showing:

  1.  The number of cycles with confirmed ovulation (`ovtoday == 1`).

  2.  The number of cycles with imputed ovulation
      (`ovtoday_impute == 1`).

- `ovstatus_id`: A participant-level summary showing, for each unique
  ID:

  1.  The number of cycles whose length falls outside 21-35 days.
      Descriptive only: a confirmed-ovulation cycle outside that range
      is still scaled, gated by its phase lengths instead (see
      `lower_cyclength_bound` in
      [`?pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)).
      Still-open trailing cycles, whose length is not yet knowable, are
      not counted.

  2.  The number of cycles with confirmed ovulation.

  3.  The number of cycles with imputed ovulation.

## Details

Specifically, it counts the number of cycles in which ovulation was:

- **Confirmed** using an ovulation biomarker (i.e., a `1` in the
  `ovtoday` column), such as urinary LH surge tests or basal body
  temperature (BBT).

- **Imputed** using the backward-count method, which estimates ovulation
  as 15 days before the subsequent menses onset (`ovtoday_impute == 1`),
  based on the typical length of the luteal phase.

The output includes both:

- Overall summary counts across the entire dataset.

- Per-individual summaries to support participant-level quality checks
  and reporting.

Report this summary in publications: it states how often ovulation
timing was measured versus estimated. Ovulation-biomarker confirmation
is preferred where available. Note that a positive LH-surge test or a
basal body temperature (BBT) nadir does not pinpoint the day of
ovulation, which would require ultrasound; it indicates that ovulation
occurred within 24 to 36 hours (Nagpal et al., 2025, Section 2.1.2), and
the width of that window depends on the detection method.

When ovulation biomarkers are unavailable, the -15 day backward count is
recommended over a cycle-midpoint assumption, because the luteal phase
varies less in length than the follicular phase. Against 33
hormone-confirmed cycles it was off by a mean absolute 0.97 days (SD
0.88; Nagpal et al., 2025, Section 3.2).

For further guidance on ovulation identification and the rationale for
the -15 day imputation method, see:

- Nagpal et al. (2025). *Studying the Menstrual Cycle as a Continuous
  Variable: Implementing Phase-Aligned Cycle Time Scaling (PACTS) with
  the `menstrualcycleR` package*. *Psychoneuroendocrinology*, 107584.
  https://doi.org/10.1016/j.psyneuen.2025.107584

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

ov_summary = summary_ovulation(data_with_scaling)
print(ov_summary)
#> $ovstatus_total
#>          Total Confirmed Ovulation
#> N cycles                        14
#>          Total Estimated Ovulation via 15day Backward Count
#> N cycles                                                 11
#> 
#> $ovstatus_id
#> # A tibble: 25 × 4
#>       id Total cycles with cycle…¹ Total cycles with co…² Total cycles with im…³
#>    <int>                     <dbl>                  <dbl>                  <dbl>
#>  1     1                         0                      0                      1
#>  2     2                         0                      1                      0
#>  3     3                         0                      1                      0
#>  4     4                         0                      0                      1
#>  5     5                         0                      1                      0
#>  6     6                         0                      0                      1
#>  7     7                         0                      1                      0
#>  8     8                         0                      1                      0
#>  9     9                         0                      0                      1
#> 10    10                         0                      1                      0
#> # ℹ 15 more rows
#> # ℹ abbreviated names: ¹​`Total cycles with cycle length < 21 or > 35`,
#> #   ²​`Total cycles with confirmed ovulation`,
#> #   ³​`Total cycles with imputed ovulation via 15day Backward Count`
#> 
ov_summary$ovstatus_total
#>          Total Confirmed Ovulation
#> N cycles                        14
#>          Total Estimated Ovulation via 15day Backward Count
#> N cycles                                                 11
ov_summary$ovstatus_id
#> # A tibble: 25 × 4
#>       id Total cycles with cycle…¹ Total cycles with co…² Total cycles with im…³
#>    <int>                     <dbl>                  <dbl>                  <dbl>
#>  1     1                         0                      0                      1
#>  2     2                         0                      1                      0
#>  3     3                         0                      1                      0
#>  4     4                         0                      0                      1
#>  5     5                         0                      1                      0
#>  6     6                         0                      0                      1
#>  7     7                         0                      1                      0
#>  8     8                         0                      1                      0
#>  9     9                         0                      0                      1
#> 10    10                         0                      1                      0
#> # ℹ 15 more rows
#> # ℹ abbreviated names: ¹​`Total cycles with cycle length < 21 or > 35`,
#> #   ²​`Total cycles with confirmed ovulation`,
#> #   ³​`Total cycles with imputed ovulation via 15day Backward Count`
```
