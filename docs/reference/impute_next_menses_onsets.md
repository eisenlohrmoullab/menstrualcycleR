# Impute a next-menses onset from a confirmed ovulation (Internal)

Implements the GENERAL next-menses imputation rule: for each
biomarker-confirmed ovulation (`ovtoday == 1`) that has NO observed
menses onset recorded anywhere after it for that person, synthesize a
menses onset at `ovulation + luteal_days` (the population-average luteal
length, so ovulation + 14 = the "LH+15"/last-follicular-day rule). This
lets a confirmed ovulation whose next menses was never recorded at all
still contribute a scalable cycle, rather than being dropped for lack of
a closing anchor.

## Usage

``` r
impute_next_menses_onsets(
  data,
  id,
  date,
  menses,
  ovtoday,
  luteal_days = 14,
  max_window = 20
)
```

## Arguments

- data:

  A data frame in long format (one row per id-date).

- id, date, menses, ovtoday:

  Unquoted column names, as in
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md).

- luteal_days:

  Days after ovulation to place the imputed onset. Default 14.

- max_window:

  Not currently used to gate the impute decision – kept for
  argument-signature stability; a non-default value has no effect on the
  result. Through 0.1.6, imputation fired whenever no OBSERVED menses
  onset fell within `max_window` days after a confirmed ovulation
  (default 20). That was a real bug, not a documented tradeoff: a
  genuine luteal phase just outside the window (confirmed on this
  package's own `cycledata` example, id 8: a real 22-day luteal phase)
  got a fabricated onset synthesized at day 14 anyway, which OVERWROTE
  that participant's actually observed `menses == 0` day to `1` –
  corrupting real data, not just adding an imputed row. As of 0.1.7 a
  real closing menses at ANY distance always prevents imputation; only a
  confirmed ovulation with NO recorded closing menses anywhere in that
  person's remaining data is treated as open. See `NEWS.md` for the full
  history.

## Value

`data` with imputed menses onsets added (existing rows updated, or new
rows appended for onset dates outside the observed range) and a
`menses_impute` indicator column.

## Details

A real, observed closing menses – at ANY distance from the ovulation,
not just a nearby one – always wins and is never overridden or
duplicated by an imputed onset (see the `max_window` entry below for why
this search is deliberately unbounded as of 0.1.7).

Study-specific gating (e.g. blocking imputation across treatment phases
or documented off-study breaks, trust-ordered de-duplication of anchors)
is deliberately NOT done here; it is the caller's responsibility to
apply on top. This function does the general rule only.

Operates on the user's own `id` / `date` / `menses` / `ovtoday` columns
and is run before cycle-length calculation, so downstream scaling sees
the augmented anchors. Imputed onsets are marked in a new
`menses_impute` column (`1` = imputed onset, `0` = everything else).
