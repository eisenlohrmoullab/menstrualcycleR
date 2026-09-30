# Changelog

## menstrualcycleR 1.1.0

### New practice dataset: `cycledata_special`

A second practice dataset ships alongside `cycledata`, built for one
job: showing what each of
[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)’s
settings actually does. It holds 15 people and 998 rows, and every
person is one special case, named in words in a new `case` column.

The dataset exists because `cycledata` cannot do this. All 25 of its
people have every cycle inside 21-35 days, not one was observed before
their first recorded menses onset, none has a confirmed ovulation with
no closing onset, and none has a calendar gap – so the settings added in
this version and in 0.1.7 change nothing at all when run on it. A
dataset in which nothing unusual happens can neither demonstrate a
setting working nor catch a claim about one that is wrong. That blind
spot is what let the ovulation-imputation behaviour of the cycle-length
bounds go undocumented for as long as it did.

The fifteen: an ordinary three-cycle record, to compare against; 8 and
then 30 rated days before a first recorded onset (the first fabricates 7
rows, the second shows how far the setting reaches); a confirmed
ovulation among the leading days, which scales with no setting at all
and makes `impute_leading_ovulation` correctly decline; a confirmed
ovulation at the end of the diary with no closing onset; an 18-day and a
42-day cycle with ovulation unconfirmed, on the wrong side of
`lower_cyclength_bound` and `upper_cyclength_bound`; a luteal phase past
`luteal_phase_max_days` and a follicular phase past
`follicular_phase_max_days`, both recovered by the phase-cap fallback
and flagged; a luteal phase under `luteal_phase_min_days`, recovered by
nothing, which is the asymmetry between the phase floors and the phase
ceilings; an eleven-day hole in a diary, to show the filled rows coming
back with no rating on them; a person with ovulation confirmed on no
day; a person with no anchor of any kind, whose columns are empty for a
reason; and a person with one confirmed ovulation and no onset ever
recorded, whose single day scales pinned at `cyclic_time = 1` and whose
whole luteal phase scales once `impute_next_menses` closes the cycle;
and a person with leading days AND an unclosed trailing ovulation, who
is the only one both opt-in rules act on at once.

Every one of those claims is checked by an automatic test rather than
only written down (`tests/testthat/test_cycledata_special.R`, 94
checks), so a change to a default that stops a person demonstrating
their setting fails a test instead of leaving a false sentence in the
help page. The dataset is generated, not real and not derived from any
real record, and the script that builds it – asserting each case’s
arithmetic as it runs – ships in the package sources at
`data-raw/cycledata_special.R`.

Both vignettes now point at it. “Preparing Your Data” gains a section,
“When your data is awkward”, that works through four of the cases with
live output: how far apart a setting’s recovered ROWS and recovered DATA
are (45 days gained, 31 of them carrying a rating, 12 rows fabricated),
that `leading_ovulation_luteal_days` scales linearly with whatever it is
passed, which person no combination of settings can reach and why, and
what the `NA`s in the `case` column are for. The overview vignette
introduces the dataset where it introduces `cycledata`.

#### Two things building it established about existing behaviour

Neither is a code change; both are corrections to what the documentation
said.

**`leading_ovulation_luteal_days` has no upper bound of any kind.** The
15 is where the ovulation is placed by default, not a cap on how far
back the rule reaches, and the documentation added with the setting in
this version implied otherwise. Setting it to 25 scales 25 leading days;
setting it to 40 scales 40 and fabricates rows before the diary begins.
The cycle-length bounds cannot gate it (a left-censored tail has no
cycle length to test, which is the reason the default declines to scale
it at all) and the phase-length caps do not: a 25-day imputed luteal
phase scales with `luteal_phase_max_days` left at 18. A value above the
default is a methods decision to record, not a recovery dial. The help
page for the new dataset now says so, and person 3 demonstrates it.

**The two opt-in rules are applied in a fixed order, and it does not
matter.** `impute_next_menses` runs first and `impute_leading_ovulation`
second, so the leading rule can see an imputed onset. On a person who
has both – person 15 of the new dataset – the days gained together are
exactly the sum of the days each gains alone. And an imputed onset can
never become a leading-day anchor, structurally: `impute_next_menses`
only imputes an onset forward from a confirmed ovulation, so whenever
that onset is a person’s first, a confirmed ovulation necessarily
precedes it, which is the condition on which the leading rule declines.

### Packaging fixes

[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)’s
help page had not been regenerated after this version’s new settings
were added, so the help page and the function disagreed about which
settings exist. `R CMD check --as-cran` reports that as a WARNING, and a
warning is a rejection.

`ovtoday_leading_impute` was missing from the list of column names the
package declares it expects, which R flags when checking the code.

Two files the testing tool writes when a check fails
(`tests/testthat/testthat-problems.rds` and `tests/testthat/_problems/`)
had been committed and would have shipped inside the package. Removed,
and excluded from future builds.

### Bug fix: first follicular phase lost at a participant boundary

The follicular-phase pass closed a participant’s run on their FIRST row
whenever the row before it – the previous participant’s last row – was
an ovulation day; the check looked at row i-1 without confirming it
belonged to the same participant. That participant’s whole first
follicular phase was then never scaled (in every column). Present in
every prior release; on a 125-participant study dataset it affected 3
participants and 43 person-days. Fixed by requiring the previous row to
belong to the same participant. Results for everyone else are unchanged
(tested against the existing suite).

### New opt-in rule: `impute_leading_ovulation`

[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
gains `impute_leading_ovulation = FALSE` (with
`leading_ovulation_luteal_days = 15`). When `TRUE`, days a participant
was observed BEFORE their first recorded menses onset – the
left-censored start of participation, which previously could never be
scaled because the luteal phase they belong to had a closing menses but
no ovulation – get an ovulation imputed at the first onset minus 15
days, the same backward count the package uses inside observed cycles.
The leading days then scale as the end of a luteal phase in
`cyclic_time_impute` / `cyclic_time_imp_ov` only; the confirmed-only
columns are untouched. The imputed anchor is marked in a new column
`ovtoday_leading_impute`, and a blank row is added when the imputed day
precedes the first observed row (as `impute_next_menses` does for an
imputed onset). Nothing is imputed when a confirmed ovulation already
lies before the first onset. The default `FALSE` keeps every existing
result byte-for-byte identical (tested). Requested by the CLEAR Lab
ADHD-Cycle analysis (2026-09-27), where roughly a third of participants
began the diary in a luteal phase.

## menstrualcycleR 1.0.0

First CRAN release. No scaled cycle-time values change and no exported
function changes behavior – the major version marks the move to CRAN,
not a break with 0.1.9. Code written against 0.1.9 runs unchanged.

### Packaging for CRAN

`cpass` is no longer listed in `Suggests`, and the `Remotes: lasy/cpass`
field is gone. CRAN does not accept a `Remotes` field, and does not
accept a suggested package that is not in a mainstream repository.
[`launch_app()`](https://menstrualcycler.clearlabresearch.com/reference/launch_app.md)
is unchanged: it still checks for both `shinyjs` and `cpass` with
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) and reports
whichever is missing, so the app’s CPASS tab behaves exactly as before
for anyone who installed `cpass` from GitHub.
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) does not
require a package to be declared, so dropping the declaration costs
nothing at run time. Only the documentation wording changed, to say that
`shinyjs` comes from CRAN and `cpass` from GitHub.

The `Description` field is rewritten. It no longer opens with the
package name, which CRAN policy disallows, and it now cites the PACTS
paper by DOI.

Three documentation links pointed at the retired
`eisenlohrmoullab.github.io` domain and now point at
`menstrualcycler.clearlabresearch.com` directly rather than relying on
the redirect. Documentation is regenerated with roxygen2 8.1.0, which
replaces the `RoxygenNote` field with `Config/roxygen2/version`.

### New vignette: Preparing Your Data for PACTS

[`vignette("preparing-your-data")`](https://menstrualcycler.clearlabresearch.com/articles/preparing-your-data.md)
covers getting data into the shape
[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
expects, and choosing among the four cycle-time variables it returns. It
starts from the case the overview vignette assumes away: a
period-tracker export listing period start dates, a separate table of
daily measurements, and no ovulation biomarker anywhere. It gives the
join that turns the first into a daily `menses` flag, notes on reshaping
Oura, Apple Health and Fitbit exports, a pre-scaling check list, and the
reporting that belongs in a methods section.

Two points in it are not stated elsewhere in the documentation.

- **Switching anchor changes no row’s inclusion.** `cyclic_time` and
  `cyclic_time_ov` scale the same rows as each other, and so do
  `cyclic_time_impute` and `cyclic_time_imp_ov`. This held on
  `cycledata` and on a copy chopped to create a left-censored opening
  tail and an open trailing cycle. Switching between confirmed-only and
  imputation-inclusive does change coverage: 358 of 744 rows against 735
  in `cycledata`, and 0 against 744 in a copy with `ovtoday` zeroed
  throughout.

- **Each variable places one anchor at zero and the other at the wrap.**
  On `cyclic_time` menses onset is at zero and ovulation is at
  `-1`/`+1`; on `cyclic_time_ov` the reverse. A feature at the wrap is
  split across the two ends of the plotted axis, so an arithmetic mean
  of its position is meaningless and the summary has to be circular.

The vignette states what the published paper does not: no confirmation
rate has been established below which ovulation-anchored questions
become unanswerable. In its place it gives the sensitivity analysis
Nagpal et al. (2025) ran on their own 44 cycles – fit on confirmed
cycles only and on the full set, and report both.

### `ovtoday = NULL` for studies with no ovulation biomarker

[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
previously required an `ovtoday` column even when none had been
measured, and the documentation told such users to fabricate a column of
zeros. Read as a hard requirement, that turned the most common
wearable-study situation into a wall.

`ovtoday` now defaults to `NULL`, meaning “no ovulation biomarker was
collected”. The column is created as all-`NA`, every cycle takes the
imputed-ovulation path, and a message says so and points at
[`summary_ovulation()`](https://menstrualcycler.clearlabresearch.com/reference/summary_ovulation.md).
Results are identical to supplying the column by hand.

Three states are kept deliberately distinct, and the distinction is the
point:

- **Omitted** is an error, with a message naming both ways forward.
  Someone who has ovulation data and simply forgot the argument must not
  silently receive an all-imputed analysis.
- **`NULL`** takes the imputed path described above.
- **A column name that is not in `data`** still errors and lists the
  available columns, so a typo cannot be mistaken for “no biomarker”.

Passing `NULL` when an `ovtoday` column exists warns, and honors the
`NULL`.

Previously, omitting the argument produced
`argument "x" is missing, with no default`, which named an internal
helper’s parameter rather than `ovtoday`.

Covered by `tests/testthat/test_ovtoday_null_path.R`, which exists
mainly to hold the three states apart. The first implementation
collapsed omitted into `NULL`, because giving the argument a default
makes
[`rlang::quo_is_missing()`](https://rlang.r-lib.org/reference/quosure-tools.html)
permanently `FALSE`; only
[`base::missing()`](https://rdrr.io/r/base/missing.html), read before
[`enquo()`](https://rlang.r-lib.org/reference/enquo.html) rebinds the
name, separates them.

### Fixes found in a pre-submission review

- **[`launch_app()`](https://menstrualcycler.clearlabresearch.com/reference/launch_app.md)
  did not check for `writexl`.** The Shiny app calls
  [`writexl::write_xlsx()`](https://docs.ropensci.org/writexl//reference/write_xlsx.html)
  for its download buttons and loads the package at startup, but
  `writexl` was declared nowhere and the dependency gate checked only
  `shinyjs` and `cpass`. A user holding both gated packages got a clean
  pass from
  [`launch_app()`](https://menstrualcycler.clearlabresearch.com/reference/launch_app.md)
  and then hit an error before the app rendered. `writexl` is now a
  suggested dependency and is checked alongside the other two.

- **[`launch_app()`](https://menstrualcycler.clearlabresearch.com/reference/launch_app.md)
  now documents its return value**, as CRAN requires of exported
  functions.

- **The vignettes required R 4.1 while `DESCRIPTION` declared 3.5.**
  Nine uses of the native `|>` pipe have been replaced with `%>%`. The
  package’s own code was already free of 4.1-only syntax, so only
  vignette building was affected, and only on R 4.0 or older.

- **`tidyverse` is no longer a suggested dependency.** The overview
  vignette loaded the whole suite but used only `dplyr` and `ggplot2`,
  both already in `Imports`. It now loads those two, and mentions
  `tidyverse` as an alternative for readers who have it.

- [`cycle_plot()`](https://menstrualcycler.clearlabresearch.com/reference/cycle_plot.md)
  used `partial = T` rather than `TRUE`.

### Documentation corrections

An audit of all package documentation against Nagpal et al. (2025)
produced the following.

- **Ovulation-biomarker precision.**
  [`?summary_ovulation`](https://menstrualcycler.clearlabresearch.com/reference/summary_ovulation.md)
  and
  [`?pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  said biomarker confirmation gives “more precise” or “greater
  precision” estimates. Section 2.1.2 of the paper says a positive
  LH-surge test and a BBT nadir do *not* pinpoint the day of ovulation,
  which would need ultrasound; they place it within 24 to 36 hours, and
  the window depends on the method. Both files now say that instead.

- **Statistical significance is not effect size.** The overview vignette
  read two significant random-effects p-values as indicating
  “meaningful” variation and heterogeneity. They indicate *detectable*
  variation at this sample size. Corrected in both places, since that
  section exists to teach GAMM output interpretation.

- **What a smoothing penalty does.** The vignette said the wiggliness
  penalty ensures the model “captures important trends without
  overfitting noise.” It now says the penalty trades bias for variance
  and can oversmooth a real feature at small n.

- **Quantified two vague claims** using the paper’s own figures: the day
  -15 backward count differs from hormone-confirmed ovulation by a mean
  absolute 0.97 days (SD 0.88) across 33 cycles, with error growing with
  cycle length (*r* = 0.395).

- **“biomarker” is now “ovulation biomarker”** in the 18 places it stood
  alone as a noun, matching the paper’s own usage in Sections 1.3 and
  2.1.1. Compound forms such as “biomarker-confirmed ovulation” are
  unchanged, since the noun already names what is measured. One section
  heading changed with its cross-reference.

Verified unchanged: every pasted statistic in the overview vignette
still matches a live knit (n = 611, R-sq.(adj) = 0.528, deviance
explained 55.5%, and both p-values).

### New section: Outstanding Questions

The overview vignette gains a section naming four unresolved areas, so
that defaults are not read as validated thresholds: statistical power
for PACTS designs, cyclical clustering and subgroup identification, the
reliability of timing features read off per-person smooths, and how much
cycle coverage a person needs before their data support an estimate.
[`cycledata_check()`](https://menstrualcycler.clearlabresearch.com/reference/cycledata_check.md)
reports coverage and deliberately sets no threshold.

### `summary_ovulation()` reports cycles outside 21-35 days

`ovstatus_id` gains a column,
`Total cycles with cycle length < 21 or > 35`. The per-cycle flag behind
it has existed since 0.1.9 but the roll-up line was commented out, so
the value was computed and discarded.

Enabling it needed one fix. The flag is `NA` on a still-open trailing
cycle, where `mcyclength_complete` is `NA` and the cycle’s length is not
yet knowable, and the roll-up used a bare
[`sum()`](https://rdrr.io/r/base/sum.html). Every participant in
`cycledata` has such a cycle, so the column would have come back `NA`
for all 25 of them. The roll-up now passes `na.rm = TRUE`.

The column is descriptive, not an inclusion criterion. A
confirmed-ovulation cycle outside 21-35 days is still scaled, gated by
its phase lengths. It is reported so that this divergence from Nagpal et
al. (2025) Section 2.1.1 is visible in output rather than only in prose
– see the `lower_cyclength_bound` documentation in
[`?pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md),
which now states the divergence explicitly, as do both vignettes and the
README quick start at the point where
[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
is called.

### Use of AI coding tools

Claude Code was used for this release: the CRAN packaging changes above,
the new vignette, and this entry. The decision to use it was made by
Dr. Tory Eisenlohr-Moul, the package maintainer, who reviewed and
approved every change.

No AI coding tool was used in the package’s initial development, which
began in January 2025. Anisha Nagpal’s contributions predate all AI tool
use: her final commit is dated 18 March 2026, and the first AI-assisted
commit is dated 30 May 2026. Of 567 commits at the time of this release,
49 carry a Claude Code `Co-Authored-By` trailer (from June 2026) and 2
are from GitHub Copilot’s coding agent (May 2026, updating
`inst/CITATION` and the startup citation in `R/zzzz.R`).

The README carries this statement permanently. No AI system is listed in
`Authors@R`.

## menstrualcycleR 0.1.9

Adds `mcyclength_complete`, returned alongside `mcyclength` from
[`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md).
Situation: on `cycle_incomplete == 1` rows, `mcyclength` is
days-observed-so-far in a still-open trailing cycle, not a cycle length.
Filtering on `mcyclength` alone, without a separate
`cycle_incomplete == 0` clause, silently admits those rows whenever the
observed-so-far count happens to land in range. Before: getting this
right required remembering that second clause every time (documented
since 0.1.8, but easy to miss – a downstream analysis filtered on
`mcyclength` alone and admitted 263 person-days from a still-open cycle
before the omission was caught). Now: `mcyclength_complete` is `NA`
whenever `cycle_incomplete == 1`, so
`filter(mcyclength_complete >= 21, mcyclength_complete <= 35)` excludes
those rows automatically, with no second clause needed. `mcyclength`
itself is unchanged.

Also fixes the same defect internally in
[`summary_ovulation()`](https://menstrualcycler.clearlabresearch.com/reference/summary_ovulation.md)’s
`cycles_outside_norm` calculation, which compared `mcyclength` (not
`mcyclength_complete`) to its `[21,35]` reference norm. This value is
currently dropped before
[`summary_ovulation()`](https://menstrualcycler.clearlabresearch.com/reference/summary_ovulation.md)
returns, so no user-visible output changes – the fix is so the
computation is correct if this column is ever exposed later, rather than
a landmine waiting to be uncovered.

One clarification while documenting this, since it is easy to over-read
the 0.1.8 correction as “the length bound never matters”:
`lower_cyclength_bound`/ `upper_cyclength_bound` fully gate cycle-time
computation for a cycle with **no** confirmed ovulation – a closed cycle
outside the bound gets zero `cyclic_time_impute` coverage, not partial,
until the bound is widened to include it. The bound does not gate a
**confirmed**-ovulation cycle, which scales on its phase lengths
regardless of `mcyclength` (see the vignette’s “Cycle Length Inclusion
Criteria” section). The published PACTS paper’s summary of this rule
(Nagpal et al., 2025, *Psychoneuroendocrinology* 181:107584, Section
2.1.1) predates the 0.1.8 correction and does not distinguish the two
cases.

Also corrects several pre-existing documentation errors found while
reviewing the above, none related to `mcyclength_complete`: the
vignette’s “Interpreting the GAMM Summary Output” section had a pasted
model-fit snapshot (n=575, R-sq.(adj)=0.533) that had drifted since the
0.1.7 phase-cap fallback and no longer matched a live run of the
vignette’s own code (n=611, R-sq.(adj)=0.528) – every number in that
section is now current. A “574” in the data-availability section
contradicted “575” from the live chunk directly above it. Three
citations
([`?pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md),
[`?summary_ovulation`](https://menstrualcycler.clearlabresearch.com/reference/summary_ovulation.md),
the vignette’s reference list) pointed at the retired OSF preprint
instead of the published paper.
[`process_luteal_phase_base()`](https://menstrualcycler.clearlabresearch.com/reference/process_luteal_phase_base.md)
was missing two `@param` entries. A couple of smaller
copy-paste/staleness fixes round it out – see the git history for the
full list.

## menstrualcycleR 0.1.8

Corrects some errors in the vignette and fixes a few small bugs. No
scaled cycle-time values change for any existing user.

To clarify how scaling eligibility works: `lower_cyclength_bound` /
`upper_cyclength_bound` determine which cycles are eligible for an
**imputed** ovulation. A cycle with a **confirmed** ovulation is scaled
based on its phase lengths (luteal 7–18 days, follicular 8–25 days, each
adjustable). The vignette now describes this accurately, and shows how
to filter by cycle length after scaling for users who want that.

Also fixes a crash affecting complete cycles of 14 days or fewer when
eligible for ovulation imputation, plus a few minor internal
corrections.

## menstrualcycleR 0.1.7

Four bug fixes that change scaled cycle-time values in specific
situations, four new arguments, input validation, and documentation
updates. Each fix states the situation it applies to.

### Bug fixes

- **Confirmed ovulation with no later menses in the data.** Situation: a
  participant’s last row is N days after a confirmed ovulation, and no
  menses onset is observed after that ovulation. Before: `cyclic_time`,
  `cyclic_time_ov`, and `luteal_length` treated day N as the last day of
  the luteal phase. Example: with N = 6, day 6 received the value of the
  day before menses and day 3 received the value of mid-luteal. Now:
  those N days are `NA` in all three columns. The same days can be
  scaled with `impute_next_menses = TRUE`, which adds an onset at
  ovulation + 14 days (LH + 15), sets `menses_impute = 1` on that row,
  and scales the days in the imputed columns. This behavior existed in
  every release before 0.1.7.

- **`POSIXct` dates.** Situation: the date column is `POSIXct` and a
  value has a non-midnight time (for example 2024-03-01 09:00). Before:
  that value became `NA` and the row was dropped; if the dropped row was
  an ovulation day, the ovulation was lost; if every row had a
  non-midnight time, the function stopped with an internal error. Now:
  the time is dropped and the date is kept. A value that cannot be read
  as a date produces a warning.

- **`impute_next_menses` with a late closing menses.** Situation:
  `impute_next_menses = TRUE`, a confirmed ovulation, and the next
  observed menses onset more than 20 days after that ovulation. Before:
  the observed onset was not found, and an imputed onset was added at
  ovulation + 14 days even though an observed onset existed. Example: an
  observed 22-day luteal phase received an imputed onset on day 14. Now:
  the search for an observed onset extends to the participant’s next
  confirmed ovulation, including an onset on that ovulation’s date, and
  no onset is imputed when one is found. `next_menses_max_window` no
  longer has an effect and warns if supplied.

- **Luteal phase over 18 days or follicular phase over 25 days, with
  confirmed ovulation.** Situation: a confirmed-ovulation cycle within
  `lower_cyclength_bound` and `upper_cyclength_bound` in which the
  luteal phase exceeds 18 days or the follicular phase exceeds 25 days.
  Before: the days beyond the cap were `NA` in `cyclic_time_impute` and
  `cyclic_time_imp_ov`, as well as in `cyclic_time` and
  `cyclic_time_ov`. Now: `cyclic_time_impute` and `cyclic_time_imp_ov`
  are computed for those days using the observed phase length, provided
  each phase is at least its minimum (7 days luteal, 8 days follicular).
  New columns `cyclic_time_impute_extended_phase` and
  `cyclic_time_imp_ov_extended_phase` equal 1 on the days filled this
  way and 0 otherwise. `cyclic_time` and `cyclic_time_ov` are unchanged.

### New arguments to `pacts_scaling()`

- `luteal_phase_min_days` (7), `luteal_phase_max_days` (18),
  `follicular_phase_min_days` (8), `follicular_phase_max_days` (25).
  These were fixed values before 0.1.7. Defaults give the same output as
  0.1.6. Raising a maximum extends `cyclic_time` and `cyclic_time_ov` to
  the longer phase. Lowering a maximum does not change
  `cyclic_time_impute` or `cyclic_time_imp_ov`, because the fallback in
  the previous bullet does not use the maximum. Details in
  [`?pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md),
  section “Internal phase-length caps”.

- Input validation. Before: a missing column stopped with an rlang
  error; `menses` or `ovtoday` coded as text (`"yes"`/`"no"`) or as
  bleeding intensity (0, 1, 2, 3) returned every derived column as `NA`
  with no message. Now: each case stops with an error that names the
  column and the problem.

### Documentation and packaging

- [`?pacts_scaling`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  documents the four new arguments, the phase-length caps, and every
  returned column including `cyclic_time`, `cyclic_time_impute`,
  `cyclic_time_ov`, and `cyclic_time_imp_ov`.
  [`?menstrualcycleR`](https://menstrualcycler.clearlabresearch.com/reference/menstrualcycleR-package.md)
  added. The vignette’s description of `impute_next_menses` updated.
- `R CMD check`: 0 errors, 0 warnings, 0 notes. Debug
  [`print()`](https://rdrr.io/r/base/print.html) output removed from
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md).
  `mgcv`, `shinyjs`, and `cpass` moved from Imports to Suggests.
  Regression tests added for each fix.

## menstrualcycleR 0.1.6

- New opt-in argument `impute_next_menses` in
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  (default `FALSE`). When `TRUE`, a next-menses onset is imputed
  *forward* from a biomarker-confirmed ovulation that has no recorded
  closing menses, placed at `ovulation + next_menses_luteal_days`
  (default 14 days, the population-average luteal length, i.e. the
  “LH+15” / last-follicular-day convention). That cycle then becomes
  scalable instead of being dropped for a missing anchor. Imputed onsets
  are flagged in a new `menses_impute` column so imputed and observed
  onsets stay distinguishable.
- Two companion arguments tune the rule: `next_menses_luteal_days`
  (default 14) and `next_menses_max_window` (default 20; skip imputation
  for an ovulation if an observed menses already closes the cycle within
  that many days).
- This complements the existing ovulation imputation, which imputes
  ovulation *backward* from an observed menses (menses minus 15). The
  package can now close a cycle from either direction when one anchor is
  missing.
- The default (`FALSE`) leaves all previous behavior byte for byte
  identical; only callers who opt in see any change. Only the general
  rule is applied. Study or protocol specific gating (for example
  blocking imputation across treatment phases or documented off-study
  breaks) remains the caller’s responsibility.

## menstrualcycleR 0.1.5

- Maintainer changed to Tory Eisenlohr-Moul.
- Documentation fixes: the `cycledata` help page now documents the
  `daterated` column (it previously said `date`, which the dataset does
  not contain), and
  [`cycle_plot()`](https://menstrualcycler.clearlabresearch.com/reference/cycle_plot.md)’s
  `align_val` argument is now documented under its correct name (was
  `alignval`).
- Build/packaging hygiene: the pkgdown site (`docs/`) and shinyapps
  deployment records (`rsconnect/`, `inst/shiny/rsconnect/`) are no
  longer bundled into the package tarball; removed a stray
  `R/.Rapp.history`; tidied a vignette chunk label that produced a
  non-portable figure filename. No effect on installed functionality.
- Internal: namespace-qualified
  [`stats::sd()`](https://rdrr.io/r/stats/sd.html)/[`stats::ave()`](https://rdrr.io/r/stats/ave.html),
  imported `rlang`’s `:=`, and registered remaining
  non-standard-evaluation column names, clearing the “no visible binding
  for global variable” check notes. No user-facing change.

## menstrualcycleR 0.1.4

- Documentation/metadata only — no code changes. Removed a dead OSF
  preprint link from the `URL:` field of `DESCRIPTION` and added a “How
  to cite” section to the README pointing to the published paper (Nagpal
  et al., 2025, *Psychoneuroendocrinology*). The canonical citation
  remains available via `citation("menstrualcycleR")` (see
  `inst/CITATION`).

## menstrualcycleR 0.1.3

- Fixed a bug in
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  where the **user’s original date column was left `NA` on fabricated
  calendar rows**. To compute cycle lengths,
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  internally densifies each participant’s calendar so that every missing
  day between their first and last observation becomes a row. Those
  fabricated rows received the package’s internal canonical `date`, but
  the column you passed in (e.g. `daterated`) was left `NA` on them.
  When a cycle’s ovulation had to be **imputed** (no biomarker-confirmed
  ovulation), the imputed-ovulation day — along with its `cyclic_time*`
  / `scaled_cycleday*` values — could land on exactly such a fabricated
  row. A downstream analysis that joined the PACTS output back to other
  data **by the original date column name** would then silently drop
  those rows, quietly losing imputed-ovulation and scaled cycle-time
  observations.
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  now returns a fully populated original date column: on fabricated rows
  it is filled from the internal calendar date, and observed dates are
  never overwritten. Character- and factor-typed date columns (a
  supported input) are coerced to `Date` the same way the rest of the
  pipeline already coerces them; `Date` and `POSIXct` date columns keep
  their type. A regression test asserts that no row carries a real
  `date` or a scaled/cycle value while the original date column is `NA`,
  across `Date`, character, factor, and `POSIXct` date inputs.

- Fixed the same class of bug for the **user’s original ID column**. The
  internal densify keeps the package’s canonical `id` populated on
  fabricated calendar rows (it is the grouping key), but an original id
  column passed under a different name (e.g. `record_id`, `subject`) was
  left `NA` on those rows — so a downstream join on **id + date** could
  still drop imputed-ovulation / scaled rows even after the date fix.
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  now also refills the original id column from the canonical `id` on
  fabricated rows (NA-fill only; observed ids are never changed and the
  column’s type is preserved). This was latent in typical use because
  the package’s own examples name the id column `id`; it surfaces for
  datasets that use any other id column name. A regression test covers
  character and integer id columns under a non-`id` name.

- Fixed a bug in
  [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  where the **last participant in a dataset** could receive `NA` for the
  scaled cycle-time variables across an entire phase (most visibly the
  luteal phase, i.e. `scaled_cycleday` after `ovtoday == 1`). The
  internal phase-length loops (`lutmax`, `folmax`, `folmax_impute` in
  `helper.R`) detected the end of a phase by looking one row ahead and
  stopped at `nrow(data) - 1`, so the final row of the dataset was never
  treated as the end of a run. The result was position-dependent:
  reordering participants so that another came last moved the problem to
  that participant. The loops now treat the final dataset row as a valid
  run-end, and a regression test asserts that a participant’s scaled
  output is identical regardless of their position in the dataset.
  Thanks to Elisabeth Conrad (Freie Universität Berlin) for the clear
  bug report and reproduction. (\[reported via PACTS user
  correspondence, June 2026\])

- Packaging hygiene: declared `purrr` in `Imports` (used in `helper.R`
  but previously undeclared), and added `tidyverse` and
  `marginaleffects` to `Suggests` so the overview vignette builds in a
  clean environment. Continuous integration (GitHub Actions
  `R-CMD-check`) now runs the full `R CMD check` — including the
  vignette — on every push and pull request.

- [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  and
  [`cycle_plot_individual()`](https://menstrualcycler.clearlabresearch.com/reference/cycle_plot_individual.md)
  no longer require `dplyr`/`tidyverse` to be attached. Several internal
  calls to dplyr verbs
  ([`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html),
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  [`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html),
  [`first()`](https://dplyr.tidyverse.org/reference/nth.html)) and to
  [`rlang::sym()`](https://rlang.r-lib.org/reference/sym.html) were not
  namespace-qualified, so a bare
  [`library(menstrualcycleR)`](https://menstrualcycler.clearlabresearch.com/)
  produced `Error: could not find function "ungroup"`. All such calls
  are now qualified (`dplyr::`/`rlang::`), and every exported function
  works with the package loaded on its own.

## menstrualcycleR 0.1.0

First release of **menstrualcycleR**, the companion R package to:

> Nagpal, A., Schmalenberger, K. M., Barone, J. C., Mulligan, E.,
> Stumper, A., Knol, L., Failenschmid, J., Kiesner, J., Peters, J. R., &
> Eisenlohr-Moul, T. A. (2025). Studying the Menstrual Cycle as a
> Continuous Variable: Implementing Phase-Aligned Cycle Time Scaling
> (PACTS) with the menstrualcycleR package. *Psychoneuroendocrinology*,
> 107584. <https://doi.org/10.1016/j.psyneuen.2025.107584>

### Core functionality

- [`pacts_scaling()`](https://menstrualcycler.clearlabresearch.com/reference/pacts_scaling.md)
  computes continuous, phase-aligned cycle-time variables — the
  recommended `cyclic_time*` measures (which map -1 and +1 to the same
  hormonal point) plus the `scaled_cycleday*` measures — centered on
  menses onset or ovulation, with optional ovulation imputation.
- [`cycle_plot()`](https://menstrualcycler.clearlabresearch.com/reference/cycle_plot.md)
  and
  [`cycle_plot_individual()`](https://menstrualcycler.clearlabresearch.com/reference/cycle_plot_individual.md)
  visualize outcomes across the standardized cycle at the group and
  individual level, with rolling-average smoothing.
- [`cycledata_check()`](https://menstrualcycler.clearlabresearch.com/reference/cycledata_check.md)
  and
  [`summary_ovulation()`](https://menstrualcycler.clearlabresearch.com/reference/summary_ovulation.md)
  summarize data availability and confirmed-versus-imputed ovulation.
- [`launch_app()`](https://menstrualcycler.clearlabresearch.com/reference/launch_app.md)
  opens an interactive Shiny app for cycle scaling, data checks,
  visualization, and C-PASS (PMDD/MRMD/PME) diagnosis.
- `cycledata` provides an example daily-diary dataset.
- Vignette *Getting Started with menstrualcycleR and Phase-Aligned Cycle
  Time Scaling (PACTS)* walks through the full workflow, including GAMM
  modeling with `mgcv` cyclic cubic regression splines.
