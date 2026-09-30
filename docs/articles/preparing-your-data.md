# Preparing Your Data for PACTS

This vignette covers getting data into the shape
[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
expects, and choosing which of its output variables to model. For what
PACTS is and why continuous cycle time outperforms day counting, see
[`vignette("menstrualcycleR-overview")`](https://menstrualcycler.clearlabresearch.com/articles/menstrualcycleR-overview.md)
or the [visual
explainer](https://menstrualcycler.clearlabresearch.com/pacts-explainer.html).

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`menstrualcycleR`](https://menstrualcycler.clearlabresearch.com/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`

## Input format

[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
takes one long-format table with four required columns.

| Column | Contents | Notes |
|----|----|----|
| `id` | Person identifier | Any type. One person may contribute many cycles. |
| `date` | Date of the observation | Class `Date`, not character. |
| `menses` | `1` on the first day of bleeding, `0` otherwise | Not every bleeding day. See Section 3. |
| `ovtoday` | `1` on the estimated day of ovulation, `0` otherwise | Required as a column even without an ovulation biomarker. See Section 4. |

Outcome columns sit alongside these.
[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
does not modify them; it appends 18 new columns, of which four are the
cycle-time variables covered in Section 7.

\
[`head`](https://rdrr.io/r/utils/head.html)`(``cycledata``, ``4``)`\
`#>   id menses ovtoday symptom  daterated`\
`#> 1  1      1       0       5 2024-01-20`\
`#> 2  1      0       0       5 2024-01-21`\
`#> 3  1      0       0       3 2024-01-22`\
`#> 4  1      0       0       2 2024-01-24`

## One row per person per day

**No duplicate `id`-`date` pairs.** Where a participant submitted two
diary entries on one day, resolve them to a single row before scaling.
Two rows for one date means that day is counted twice in every
downstream model.

\
`dups`` ``<-`` ``cycledata`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`count`](https://dplyr.tidyverse.org/reference/count.html)`(``id``, ``daterated``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``n`` ``>`` ``1``)`\
[`nrow`](https://rdrr.io/r/base/nrow.html)`(``dups``)`\
`#> [1] 0`

**Gaps need no filling.**
[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
fills the calendar internally so that cycle position is defined for
every date inside a cycle. It therefore returns more rows than it was
given, and the added rows carry a date and a cycle-time value but no
outcome. `cycledata` goes in with 619 rows and comes out with 744, so
125 rows (17% of the output) have no `symptom` value. Count coverage
over rows where the outcome is observed, not over all rows.

\
`scaled`` ``<-`` `[`pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)`(``cycledata``, id ``=`` ``id``, date ``=`` ``daterated``,`\
`                        menses ``=`` ``menses``, ovtoday ``=`` ``ovtoday``)`\
\
[`c`](https://rdrr.io/r/base/c.html)`(``all_rows             ``=`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``scaled``)``,`\
`  rows_with_an_outcome ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``scaled``$``symptom``)``)``)`\
`#>             all_rows rows_with_an_outcome `\
`#>                  744                  619`

Any per-person day count or sparsity threshold should use the second
number.

The `lower_cyclength_bound` / `upper_cyclength_bound` arguments default
to 21 and 35 days and gate **ovulation imputation** only. A cycle with a
confirmed ovulation is scaled whatever its length, subject to its phase
lengths instead. Nagpal et al. (2025) §2.1.1 describes the bounds as
deciding which cycles are scaled at all; the package applies the
carve-out named in that section’s next sentence, and does so by default
rather than on request.
[`summary_ovulation()`](https://menstrualcycler.clearlabresearch.com/reference/summary_ovulation.md)
reports how many cycles fall outside 21-35 days. To exclude them, filter
on `mcyclength_complete` after scaling.

## Menses onset

`menses == 1` marks the first day of a period; every later bleeding day
is `0`. Coding all bleeding days as `1` makes the function read a
four-day period as four cycle onsets, and nothing errors.

Exclude periovulatory spotting, which is not a menses onset and will
start a phantom cycle. Reclassify post-midnight entries to the previous
day before scaling, so the diary and any sleep or wearable data agree on
which day is which.

## Ovulation

`ovtoday` marks the estimated day of ovulation. Ovulation biomarkers are
preferred, and the coding convention differs by method.

| Method | Code `ovtoday = 1` on | Report |
|----|----|----|
| Urinary LH test | The day after the first positive test (LH+1) | Brand and threshold, e.g. 40 mIU/ml |
| Basal body temperature | The day after the temperature nadir | How temperature was measured |
| Daily hormone assay | See Nagpal et al. (2025) | Assay and analyte |
| None available | Pass `ovtoday = NULL` | The imputation rate (Section 7) |

With only period-tracker dates, pass `ovtoday = NULL`. The column is
created for you as all-`NA`,
[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
places ovulation 15 days before each next menses onset, records it in
`ovtoday_impute`, and covers those cycles in the `*_impute` cycle-time
columns. A message reports that this happened. Supplying your own column
of all `0`s or `NA`s does the same thing.

Omitting the argument altogether is an error rather than a silent
fallback, so that forgetting it when you *do* have ovulation data cannot
quietly produce an all-imputed analysis. Against 33 hormone-confirmed
cycles the imputed estimate was off by a mean absolute 0.97 days
(Section 7.2).

## From a period-tracker export to the PACTS table

The common starting point is one table of period start dates and a
separate table of daily measurements.

\
`period_log`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  id           ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``1``, ``1``, ``2``, ``2``, ``2``)``,`\
`  period_start ``=`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"2024-01-03"``, ``"2024-01-31"``, ``"2024-02-27"``,`\
`                           ``"2024-01-08"``, ``"2024-02-10"``, ``"2024-03-09"``)``)`\
`)`\
\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`\
`wearable`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  id   ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``2``)``, each ``=`` ``75``)``,`\
`  date ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`seq`](https://rdrr.io/r/base/seq.html)`(`[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2024-01-01"``)``, by ``=`` ``"day"``, length.out ``=`` ``75``)``,`\
`           `[`seq`](https://rdrr.io/r/base/seq.html)`(`[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2024-01-05"``)``, by ``=`` ``"day"``, length.out ``=`` ``75``)``)``,`\
`  resting_hr ``=`` `[`round`](https://rdrr.io/r/base/Round.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(`[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``75``, ``58``, ``2``)``, `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``75``, ``62``, ``2``)``)``, ``1``)`\
`)`\
\
[`head`](https://rdrr.io/r/utils/head.html)`(``period_log``, ``3``)`\
`#>   id period_start`\
`#> 1  1   2024-01-03`\
`#> 2  1   2024-01-31`\
`#> 3  1   2024-02-27`\
[`head`](https://rdrr.io/r/utils/head.html)`(``wearable``, ``3``)`\
`#>   id       date resting_hr`\
`#> 1  1 2024-01-01       56.7`\
`#> 2  1 2024-01-02       58.4`\
`#> 3  1 2024-01-03       56.3`

A left join turns the period log into a daily flag.

\
`daily`` ``<-`` ``wearable`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``period_log``, menses ``=`` ``1L``)``,`\
`            by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"id"``, ``"date"`` ``=`` ``"period_start"``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    menses  ``=`` `[`coalesce`](https://dplyr.tidyverse.org/reference/coalesce.html)`(``menses``, ``0L``)``,`\
`    ovtoday ``=`` ``0L``          ``# no ovulation biomarker in this example`\
`  ``)`\
\
`daily`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``menses`` ``==`` ``1``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`head`](https://rdrr.io/r/utils/head.html)`(``3``)`\
`#>   id       date resting_hr menses ovtoday`\
`#> 1  1 2024-01-03       56.3      1       0`\
`#> 2  1 2024-01-31       60.7      1       0`\
`#> 3  1 2024-02-27       55.9      1       0`

\
`daily_scaled`` ``<-`` `[`pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)`(``daily``, id ``=`` ``id``, date ``=`` ``date``,`\
`                              menses ``=`` ``menses``, ovtoday ``=`` ``ovtoday``)`\
\
`daily_scaled`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``id``, ``date``, ``resting_hr``, ``cyclic_time_impute``, ``ovtoday_impute``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``cyclic_time_impute``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``4``)`\
`#> ``# A tibble: 4 × 5`\
`#>      id date       resting_hr cyclic_time_impute ovtoday_impute`\
`#>   ``<dbl>`` ``<date>``          ``<dbl>``              ``<dbl>``          ``<int>`\
`#> ``1``     1 2024-01-03       56.3             0                   0`\
`#> ``2``     1 2024-01-04       61.2             0.076``9``              0`\
`#> ``3``     1 2024-01-05       58.7             0.154               0`\
`#> ``4``     1 2024-01-06       56.4             0.231               0`

### Common device exports

Reshape each source into `id`, `date`, value form before running the
join above.

- **Oura.** Daily sleep and readiness endpoints already give one row per
  day. Use the sleep `day` field rather than the timestamp, so nights
  are attributed to the day the person woke.
- **Apple Health.** Exports are per-sample. Aggregate to a daily value
  first, choosing explicitly between a daily mean, a nightly minimum, or
  a single reading. Apple Health also records `MenstrualFlow`, which can
  supply `menses` if reduced to the first day of each run of flow days.
- **Fitbit.** Daily summaries arrive as one row per day per metric, in
  separate files. Join them on date before joining to the period log.
- **Period-tracker apps.** Most export one row per bleeding day rather
  than one per period. Reduce to onsets first by keeping a day only when
  the previous calendar day was not also a bleeding day.

## Checks before scaling

\
`checks`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  date_is_Date        ``=`` `[`inherits`](https://rdrr.io/r/base/class.html)`(``daily``$``date``, ``"Date"``)``,`\
`  no_duplicate_rows   ``=`` ``!`[`any`](https://rdrr.io/r/base/any.html)`(`[`duplicated`](https://rdrr.io/r/base/duplicated.html)`(``daily``[`[`c`](https://rdrr.io/r/base/c.html)`(``"id"``, ``"date"``)``]``)``)``,`\
`  menses_is_binary    ``=`` `[`all`](https://rdrr.io/r/base/all.html)`(``daily``$``menses`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``1``)``)``,`\
`  has_ovtoday_column  ``=`` ``"ovtoday"`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`names`](https://rdrr.io/r/base/names.html)`(``daily``)``,`\
`  every_id_has_onset  ``=`` `[`all`](https://rdrr.io/r/base/all.html)`(`[`tapply`](https://rdrr.io/r/base/tapply.html)`(``daily``$``menses``, ``daily``$``id``, ``sum``)`` ``>`` ``0``)`\
`)`\
`checks`\
`#>       date_is_Date  no_duplicate_rows   menses_is_binary has_ovtoday_column `\
`#>               TRUE               TRUE               TRUE               TRUE `\
`#> every_id_has_onset `\
`#>               TRUE`

A `FALSE` on the last check usually means a person’s period dates failed
to join, most often because the identifier is a factor in one table and
a character in the other.

After scaling,
[`cycledata_check()`](https://menstrualcycler.clearlabresearch.com/reference/cycledata_check.md)
reports how much usable outcome data each person has in each phase,
which feeds any inclusion rule.

It imposes no threshold. How much coverage a person needs before their
scaled data support an estimate is an open question, along with power,
cyclical clustering, and the reliability of per-person smooths. See
“Outstanding Questions” in
[`vignette("menstrualcycleR-overview")`](https://menstrualcycler.clearlabresearch.com/articles/menstrualcycleR-overview.md).

## Choosing a cycle-time variable

[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
returns four cycle-time variables, crossing the anchor with whether
imputed ovulation is included.

|  | Confirmed ovulation only | Imputed ovulation included |
|----|----|----|
| **Centered on menses onset** | `cyclic_time` | `cyclic_time_impute` |
| **Centered on ovulation** | `cyclic_time_ov` | `cyclic_time_imp_ov` |

Switching anchor changes no row’s inclusion. Switching between
confirmed-only and imputation-inclusive changed coverage from 358 rows
to 735 in `cycledata`, and from 0 rows to 744 in a copy of it with
`ovtoday` zeroed.

### Anchor

Center on the event the outcome is tied to. Perimenstrual outcomes such
as dysmenorrhea belong on the menses-centered variables, periovulatory
outcomes such as reward sensitivity on the ovulation-centered ones.
Nagpal et al. (2025) recommend fitting both in separate models where the
expected timing is unclear.

Each variable places one anchor at zero and the other at the endpoints,
`-1` and `+1`. On `cyclic_time`, menses onset is at zero and ovulation
is at the wrap; on `cyclic_time_ov`, the reverse. A feature sitting at
the wrap is split across the two ends of the plotted axis, and any
summary of where it falls has to be circular rather than linear, since
an arithmetic mean of positions straddling `-1` and `+1` is meaningless.
Nagpal et al. (2025) additionally flag, as an open limitation rather
than a settled result, that centering on a hormonal transition may
reduce sensitivity to change at the endpoints, and suggest alternative
centering such as the mid-follicular phase as future work.

Fit both models when the outcome has features at both anchors, such as a
perimenstrual rise and a periovulatory dip, and read each feature from
the fit that centers it.

`cyclic_time` and `cyclic_time_ov` cover the same rows as each other,
and so do `cyclic_time_impute` and `cyclic_time_imp_ov`. This held on
`cycledata` and on a copy chopped to create a left-censored opening tail
and an open trailing cycle.

Having no ovulation biomarker does not change which anchor to pick, but
it does put a measured event at zero in one case and an estimated one at
zero in the other. The imputed ovulation day is off by a mean absolute
0.97 days (Section 7.2). Spread over a follicular phase averaging 14.8
days, that is 0.066 of the 0-to-1 follicular span, so a feature narrower
than about 0.07 on the axis cannot be located reliably in an
ovulation-centered fit. State in the caption that zero is an estimated
day.

### Confirmed or imputed ovulation

In `cycledata`, 14 of 25 cycles have a confirmed ovulation.
`cyclic_time` scales 358 of 744 rows and `cyclic_time_impute` scales
735. Zeroing `ovtoday` throughout drops `cyclic_time` to 0 rows and
raises `cyclic_time_impute` to 744, because every cycle then takes the
imputed path.

Run this on your own data before choosing.

\
`vars`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"cyclic_time"``, ``"cyclic_time_impute"``, ``"cyclic_time_ov"``, ``"cyclic_time_imp_ov"``)`\
[`sapply`](https://rdrr.io/r/base/lapply.html)`(``vars``, ``function``(``v``)`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``scaled``[[``v``]``]``)``)``)`\
`#>        cyclic_time cyclic_time_impute     cyclic_time_ov cyclic_time_imp_ov `\
`#>                358                735                358                735`

Model `cyclic_time` or `cyclic_time_ov` when the confirmed-only coverage
is enough for the analysis, and report the confirmation rate either way.
When it is not, use the `*_impute` variables rather than dropping the
unconfirmed cycles: the alternative to an imputed ovulation is a forward
day count, which Nagpal et al. (2025) found had the highest
within-person residual variance of the five time variables they
compared.

Against 33 hormone-confirmed cycles, the day −15 backward count differed
from the hormone-confirmed day of ovulation by a mean absolute 0.97 days
(SD 0.88), with a mean signed difference of 0.30 days (SD 1.29). The
error grew with cycle length in the same sample (*r* = 0.395, *p* =
.023), so the estimate is least accurate in the longest cycles. PACTS
has not been validated in samples with highly variable cycle lengths,
frequent anovulation, or conditions that alter hormone dynamics.

### Sensitivity to imputed ovulation

No confirmation rate has been established below which ovulation-anchored
questions become unanswerable. The published guidance is to acknowledge
the imprecision and report the proportion of cycles in which ovulation
was estimated rather than measured.

Fit the model twice whenever any cycle in the analysis used an imputed
ovulation: once on confirmed cycles only, once on the full set. Nagpal
et al. (2025) report that their confirmed-only analyses agreed with the
combined dataset, which is what justified pooling those 44 cycles. That
agreement is a property of a particular sample, not of the method.

\
`# The confirmed-only subset is the rows cyclic_time covers.`\
[`c`](https://rdrr.io/r/base/c.html)`(``rows_confirmed_only ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``scaled``$``cyclic_time``)``)``,`\
`  rows_incl_imputed   ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``scaled``$``cyclic_time_impute``)``)``)`\
`#> rows_confirmed_only   rows_incl_imputed `\
`#>                 358                 735`

Report both fits in the methods, including when they agree.

### Extended-phase rows

This filter is package behavior rather than published method. When a
cycle’s luteal or follicular phase falls outside the plausible bounds,
the `*_impute` variables still cover it, using phase fractions computed
without the upper phase-length cap. Those rows carry a flag.

\
`strict`` ``<-`` `[`subset`](https://rdrr.io/r/base/subset.html)`(``scaled``, ``cyclic_time_impute_extended_phase`` ``==`` ``0``)`

In `cycledata` this flag is set on 5.4% of scaled rows. The fallback
only adds coverage and has no argument that disables it, so filter after
scaling if you do not want those rows.

## When your data is awkward

Everything above assumes a diary that behaves: a first period onset
before the first rating, a closing onset after the last one, every cycle
of a plausible length. Real diaries break all three, and
[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
has a setting for most of the ways they break. The package ships a
second practice dataset for exactly this, in which every person is one
awkward case, named in a `case` column.

\
`cases`` ``<-`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``cycledata_special``[``, `[`c`](https://rdrr.io/r/base/c.html)`(``"id"``, ``"case"``)``]``)`\
[`writeLines`](https://rdrr.io/r/base/writeLines.html)`(`[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"%2d  %s"``, ``cases``$``id``, ``cases``$``case``)``)`\
`#>  1  ordinary: three complete cycles, ovulation confirmed`\
`#>  2  8 rated days before the first recorded menses onset`\
`#>  3  30 rated days before the first onset: how many scale is a setting, not a cap`\
`#>  4  confirmed ovulation among the leading days: scales with no setting`\
`#>  5  confirmed ovulation at the end, no closing menses onset`\
`#>  6  an 18-day cycle, ovulation not confirmed: under lower_cyclength_bound`\
`#>  7  a 42-day cycle, ovulation not confirmed: over upper_cyclength_bound`\
`#>  8  confirmed ovulation 22 days before the next onset: past luteal_phase_max_days`\
`#>  9  confirmed ovulation 26 days after the onset: past follicular_phase_max_days`\
`#> 10  confirmed ovulation 5 days before the next onset: under luteal_phase_min_days`\
`#> 11  an 11-day hole in the diary: rows come back with no rating`\
`#> 12  ovulation confirmed on no day: imputed columns only`\
`#> 13  no menses onset and no confirmed ovulation: nothing can scale`\
`#> 14  confirmed ovulation but no onset ever: one day scales, or a whole luteal phase`\
`#> 15  both opt-in rules at once, at opposite ends of one diary`

Run your own settings against it before switching them on for your data.
The point is not that these people are unusual – in the CLEAR lab’s
merged research data, 57% of participants have rated days before their
first recorded period – but that in `cycledata` none of them occur, so
nothing there shows a setting working or failing.

### A setting recovers rows and data at different rates

An imputed anchor can fall earlier than a participant’s first rating,
and the package then fills the calendar up to it. Those extra rows are
output rows with no outcome on them, so “days recovered” and “data
recovered” are two numbers, and the smaller one is the one to report.

\
`off`` ``<-`` `[`pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)`(``cycledata_special``, id ``=`` ``id``, date ``=`` ``daterated``,`\
`                     menses ``=`` ``menses``, ovtoday ``=`` ``ovtoday``)`\
\
`on`` ``<-`` `[`pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)`(``cycledata_special``, id ``=`` ``id``, date ``=`` ``daterated``,`\
`                    menses ``=`` ``menses``, ovtoday ``=`` ``ovtoday``,`\
`                    impute_leading_ovulation ``=`` ``TRUE``)`\
\
`covered`` ``<-`` ``function``(``x``)`` ``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``x``$``cyclic_time_impute``)`\
\
[`c`](https://rdrr.io/r/base/c.html)`(``days_gained     ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``covered``(``on``)``)`` ``-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``covered``(``off``)``)``,`\
`  with_a_rating   ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``covered``(``on``)`` ``&`` ``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``on``$``symptom``)``)`` ``-`\
`                    `[`sum`](https://rdrr.io/r/base/sum.html)`(``covered``(``off``)`` ``&`` ``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``off``$``symptom``)``)``,`\
`  rows_fabricated ``=`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``on``)`` ``-`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``off``)``)`\
`#>     days_gained   with_a_rating rows_fabricated `\
`#>              45              31              12`

The same holds for `impute_next_menses`, which closes a cycle forward
from a confirmed ovulation that never got a closing period.

### `leading_ovulation_luteal_days` is a placement, not a cap

The 15 is where the ovulation is put by default. It is not a ceiling on
how far back the rule reaches, and nothing else bounds it either: the
cycle-length bounds cannot, because a tail with no opening onset has no
cycle length to test, and the phase-length caps are not consulted on
this path at all.

\
[`sapply`](https://rdrr.io/r/base/lapply.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``15``, ``25``, ``40``)``, ``function``(``k``)`` ``{`\
`  ``x`` ``<-`` `[`pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)`(``cycledata_special``, id ``=`` ``id``, date ``=`` ``daterated``,`\
`                     menses ``=`` ``menses``, ovtoday ``=`` ``ovtoday``,`\
`                     impute_leading_ovulation ``=`` ``TRUE``,`\
`                     leading_ovulation_luteal_days ``=`` ``k``)`\
`  `[`sum`](https://rdrr.io/r/base/sum.html)`(``covered``(``x``)``)`` ``-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``covered``(``off``)``)`\
`}``)`\
`#> [1]  15  45  75 120`

Every extra day you ask for is scaled, in a straight line, with no gate
anywhere. Treat a value above the default as a methods decision to write
down, not a coverage dial to turn up.

### Some people cannot be recovered, and it is worth knowing which

Turn on both opt-in rules, widen the cycle-length bounds to the CLEAR
lab’s `[20, 43]`, and drop the luteal floor, and one person in fifteen
still has no cycle time at all.

\
`most`` ``<-`` `[`pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)`(``cycledata_special``, id ``=`` ``id``, date ``=`` ``daterated``,`\
`                      menses ``=`` ``menses``, ovtoday ``=`` ``ovtoday``,`\
`                      impute_leading_ovulation ``=`` ``TRUE``, impute_next_menses ``=`` ``TRUE``,`\
`                      lower_cyclength_bound ``=`` ``20``, upper_cyclength_bound ``=`` ``43``,`\
`                      luteal_phase_min_days ``=`` ``4``)`\
\
[`unique`](https://rdrr.io/r/base/unique.html)`(``most``$``case``[``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``most``$``id``)`` ``&`` `[`is.na`](https://rdrr.io/r/base/NA.html)`(``most``$``cyclic_time_impute``)`` ``&`\
`                   ``!``most``$``id`` `[`%in%`](https://rdrr.io/r/base/match.html)` ``most``$``id``[``covered``(``most``)``]``]``)`\
`#> [1] "no menses onset and no confirmed ovulation: nothing can scale"`

That person has no menses onset and no confirmed ovulation, so there is
nothing to anchor to and no setting can invent one. Empty cycle-time
columns for a participant like this are the correct answer, not a bug –
and
[`?cycledata_special`](https://menstrualcycler.clearlabresearch.com/reference/cycledata_special.md)
lists which of the other fourteen are recoverable, by which setting, and
at what cost.

### The `case` column disappears on fabricated rows, deliberately

`case` is a per-person label, so it reads `NA` on exactly the rows the
package added – calendar-filled gaps and imputed anchors. That makes it
the cheapest possible check of which rows in your own output nobody
rated. Carry it across those rows when you want it for grouping.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyr`](https://tidyr.tidyverse.org)`)`\
`most`` ``|>`` `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``id``)`` ``|>`` `[`fill`](https://tidyr.tidyverse.org/reference/fill.html)`(``case``, .direction ``=`` ``"downup"``)`` ``|>`` `[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`

## What to report

**How ovulation was determined.**
[`summary_ovulation()`](https://menstrualcycler.clearlabresearch.com/reference/summary_ovulation.md)
splits cycles into biomarker-confirmed and imputed.

\
[`summary_ovulation`](https://menstrualcycler.clearlabresearch.com/reference/summary_ovulation.md)`(``scaled``)``$``ovstatus_total`\
`#>          Total Confirmed Ovulation`\
`#> N cycles                        14`\
`#>          Total Estimated Ovulation via 15day Backward Count`\
`#> N cycles                                                 11`

**What share of coverage came from the phase-length fallback.**

\
[`mean`](https://rdrr.io/r/base/mean.html)`(``scaled``$``cyclic_time_impute_extended_phase`` ``==`` ``1``, na.rm ``=`` ``TRUE``)`\
`#> [1] 0.05376344`

## References

Nagpal, A., Schmalenberger, K. M., Barone, J. C., Mulligan, E., Stumper,
A., Knol, L., Failenschmid, J., Kiesner, J., Peters, J. R., &
Eisenlohr-Moul, T. A. (2025). Studying the menstrual cycle as a
continuous variable: Implementing Phase-Aligned Cycle Time Scaling
(PACTS) with the `menstrualcycleR` package. *Psychoneuroendocrinology*,
107584. <https://doi.org/10.1016/j.psyneuen.2025.107584>

Schmalenberger, K. M., et al. (2021). How to study the menstrual cycle:
Practical tools and recommendations. *Psychoneuroendocrinology, 123*,
104895. <https://doi.org/10.1016/j.psyneuen.2020.104895>

------------------------------------------------------------------------

*This vignette was drafted with Claude Code and reviewed by the package
authors, who are responsible for its content. See the README for the
package’s full statement on AI tool use.*
