# menstrualcycleR 0.1.6

## New: optional next-menses imputation

This release adds a single, opt-in capability to
[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
and changes nothing else.

### What it does

PACTS has always been able to impute a missing **ovulation** by counting
backward from an observed menses (ovulation is placed 15 days before the
next period). Version 0.1.6 adds the mirror case. When a cycle has a
biomarker-confirmed **ovulation** but the **next menses onset was never
recorded** (for example, the participant stopped surveying, or the
closing period was missed), that cycle can now be closed by imputing a
menses onset forward from the ovulation.

Set `impute_next_menses = TRUE`:

``` r

pacts_scaling(
  data, id, date, menses, ovtoday,
  impute_next_menses = TRUE
)
```

The imputed onset is placed at ovulation plus 14 days (the
population-average luteal length, i.e. the last follicular day, the
“LH+15” convention). Imputation is skipped when an observed menses
already closes the cycle within 20 days, so a recorded period is never
overridden. Both windows are adjustable via `next_menses_luteal_days`
(default 14) and `next_menses_max_window` (default 20).

Imputed onsets are flagged in a new `menses_impute` column, exactly the
way imputed ovulations are flagged in `ovtoday_impute`, so you can
report imputed and observed onsets separately.

### Why it matters

Confirmed-ovulation cycles that lacked a recorded closing period used to
be dropped for want of an anchor. Opting in recovers those cycles, which
increases usable coverage in `cyclic_time_impute` without loss of
precision on the confirmed ovulation itself.

### Backward compatibility

`impute_next_menses` defaults to `FALSE`, so every existing script and
every current user gets byte-for-byte identical output. Only callers who
opt in see any change.

### Scope

The package applies the general rule only. Study-specific or
protocol-specific gating (for example, not imputing across a treatment
phase or a documented off-study break, or trust-ordered de-duplication
of anchors) stays with the caller, so labs can layer their own rules on
top of this option.

### How to update

``` r

# from GitHub
remotes::install_github("eisenlohrmoullab/menstrualcycleR")
```

See `NEWS.md` for the full changelog and
[`?pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
for the argument documentation.
