# Impute a leading ovulation anchor before a participant's first recorded menses onset

Days observed BEFORE a participant's first recorded menses onset (the
left-censored start of participation) have no cycle anchor on their
left, so PACTS cannot place them: the luteal phase they belong to has a
closing menses (the first onset) but no ovulation. This opt-in rule
imputes that missing ovulation with the same population-average backward
count the package already uses inside observed cycles: ovulation is
placed `luteal_days` days (default 15) before the first menses onset, so
the leading days scale as the end of a luteal phase in
`cyclic_time_impute` / `cyclic_time_imp_ov` (never in the confirmed-only
columns). The anchor is marked in a new column `ovtoday_leading_impute`
(1 on the imputed day, 0 elsewhere). If the imputed day lies before the
first observed row, a blank row is added on that date (other columns
`NA`), exactly as
[`impute_next_menses_onsets()`](https://menstrualcycler.clearlabresearch.com/reference/impute_next_menses_onsets.md)
adds a blank row for an imputed onset.

## Usage

``` r
impute_leading_ovulation_anchors(
  data,
  id,
  date,
  menses,
  ovtoday,
  luteal_days = 15
)
```

## Arguments

- data, id, date, menses, ovtoday:

  As in
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md).

- luteal_days:

  Integer; days before the first menses onset at which the ovulation is
  placed. Default 15 (the package's backward-count convention).

## Value

`data` with an added integer column `ovtoday_leading_impute`, and
possibly one added row per participant on the imputed date.

## Details

Nothing is imputed when the participant has no menses onset, when no
observed row precedes the first onset, or when a confirmed ovulation
(`ovtoday == 1`) already lies before the first onset. Only the general
rule is applied; study-specific gating is the caller's responsibility.
