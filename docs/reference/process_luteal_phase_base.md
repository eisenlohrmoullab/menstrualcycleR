# Process Data (Internal)

Internal helper functions to process the data.

## Usage

``` r
process_luteal_phase_base(
  data,
  id,
  date,
  menses,
  luteal_phase_min_days = 7,
  luteal_phase_max_days = 18
)
```

## Arguments

- data:

  A data frame containing cycle data.

- id:

  A column specifying individual ids.

- date:

  A column specifying the dates.

- menses:

  A column indicating menses (0/1).

- luteal_phase_min_days, luteal_phase_max_days:

  Numeric bounds (days) on how long a confirmed ovulation's luteal phase
  (ovulation to next menses) may be for `cyclic_lut`/
  `cyclic_time`/`luteal_length` to scale it. Defaults `7` and `18` (Bull
  et al. 2019 norms); see
  [`?pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)'s
  "Internal phase-length caps" section.

## Value

A data frame with processed data.
