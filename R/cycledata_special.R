#' Practice dataset of special cases: one person per setting
#'
#' A second, deliberately awkward practice dataset. Where [cycledata] is an
#' ordinary daily diary, every person here exists to exercise exactly one
#' situation that `pacts_scaling()` has a setting for, or one situation no
#' setting can rescue. The `case` column names which. Use it to see what a
#' setting actually changes before switching it on for your own data, and to
#' recognise a person whose columns come back empty for a reason.
#'
#' `cycledata` cannot do this job: all 25 of its people have every cycle in
#' range, none was observed before their first recorded menses onset, none has
#' a right-censored luteal phase, and none has a calendar gap. A dataset in
#' which nothing unusual happens cannot show a setting working, and cannot
#' catch a claim about one that is wrong.
#'
#' @format A data frame with 998 rows and 6 variables:
#' \describe{
#'   \item{id}{Participant ID, 1 to 15. One person per case.}
#'   \item{daterated}{Date of observation}
#'   \item{symptom}{Daily rating, 0 to 10, with a perimenstrual peak. `NA` on
#'     roughly 8 percent of days, at random, so that coverage figures have to
#'     be counted over days that were actually rated.}
#'   \item{menses}{`1` on the first day of menses, `0` otherwise}
#'   \item{ovtoday}{`1` on a biomarker-confirmed day of ovulation, `0` otherwise}
#'   \item{case}{Which special case this person is, in words. \strong{Reads
#'     `NA` on rows the package itself adds} -- calendar-filled gaps and
#'     imputed anchors -- which is deliberate: it is the cheapest way to see
#'     which rows in your output nobody rated. Carry it across those rows with
#'     \code{dplyr::group_by(id) |> tidyr::fill(case, .direction = "downup")}.}
#' }
#'
#' @section The fifteen cases:
#' Figures are from a run at the default settings unless a setting is named.
#' "Days covered" counts rows with a non-missing value, out of all rows for
#' that person after the package fills the calendar.
#' \describe{
#'   \item{1. Ordinary}{Three complete cycles, ovulation confirmed, every phase
#'     in range. The baseline: without it you cannot tell a setting that
#'     recovered nothing from a dataset that is broken.}
#'   \item{2. Eight rated days before the first recorded onset}{With
#'     `impute_leading_ovulation = TRUE`, `cyclic_time_impute` gains 15 days --
#'     but 7 of those rows are ones the package fabricated, because the imputed
#'     ovulation lands a week before this diary begins. Gains more rows than
#'     data, which is the caution that matters for this setting.}
#'   \item{3. Thirty rated days before the first onset}{The same setting gains
#'     15 of those 30 days and fabricates nothing. \strong{The 15 is a
#'     placement, not a cap}, which this person is here to show:
#'     `leading_ovulation_luteal_days = 25` scales 25 days, `= 40` scales 40 and
#'     fabricates 10 rows before the diary begins, and the phase-length caps
#'     never gate any of it (a 25-day imputed luteal phase scales with
#'     `luteal_phase_max_days` left at its default of 18). So this setting has
#'     no upper bound of any kind -- not the cycle-length bounds, which need a
#'     cycle length there is none of, and not the phase caps. What limits it is
#'     the number you pass.}
#'   \item{4. A confirmed ovulation among the leading days}{Nothing is imputed
#'     (the setting declines when a confirmed ovulation is already there), and
#'     those days scale anyway, in the confirmed-only columns, with no setting
#'     switched on. The six days before that ovulation have no onset on their
#'     left, so no follicular fraction exists for them and they stay empty.}
#'   \item{5. A confirmed ovulation at the end, no closing onset}{With
#'     `impute_next_menses = TRUE`, the cycle closes 14 days after that
#'     ovulation: 14 days are gained in `cyclic_time` as well as
#'     `cyclic_time_impute`, and 5 rows are fabricated to reach the imputed
#'     onset.}
#'   \item{6. An 18-day cycle, ovulation not confirmed}{Shorter than
#'     `lower_cyclength_bound`, so no ovulation is imputed and the cycle has no
#'     cycle time at all -- while the 28-day cycles either side of it do.}
#'   \item{7. A 42-day cycle, ovulation not confirmed}{Longer than
#'     `upper_cyclength_bound`, same result. Widening the two bounds to
#'     `c(18, 43)` imputes an ovulation for this cycle and person 6's, and
#'     recovers 58 days between them.}
#'   \item{8. Confirmed ovulation 22 days before the next onset}{The luteal
#'     phase runs past `luteal_phase_max_days`, inside a 35-day cycle that is
#'     still within the cycle-length bounds. `cyclic_time` covers 14 days;
#'     `cyclic_time_impute` covers 36, recovering the luteal phase through the
#'     phase-cap fallback and flagging 21 of those rows in
#'     `cyclic_time_impute_extended_phase`. `luteal_phase_max_days = 25`
#'     returns all 22 days to `cyclic_time` itself.}
#'   \item{9. Confirmed ovulation 26 days after the onset}{The same, on the
#'     follicular side of `follicular_phase_max_days`: 10 days in
#'     `cyclic_time`, 36 in `cyclic_time_impute`, 26 flagged.
#'     `follicular_phase_max_days = 30` returns all 26.}
#'   \item{10. Confirmed ovulation 5 days before the next onset}{Under
#'     `luteal_phase_min_days`, and this is the asymmetry: the fallback ignores
#'     the phase \emph{ceilings} but still applies the \emph{floors}, so unlike
#'     people 8 and 9 these days are empty in every column. Only lowering
#'     `luteal_phase_min_days` recovers them.}
#'   \item{11. An eleven-day hole in the diary}{Eleven days with no rows at
#'     all, swallowing neither an onset nor an ovulation. They come back in the
#'     output with no rating on them, which is why a coverage figure counted
#'     over output rows overstates what you have.}
#'   \item{12. Ovulation confirmed on no day}{Every cycle in range, so every
#'     cycle takes the imputed path: `cyclic_time` is empty for this person and
#'     `cyclic_time_impute` covers 86 days. The confirmed-versus-imputed
#'     contrast, in one person.}
#'   \item{13. No onset and no confirmed ovulation}{No anchor of any kind.
#'     Nothing scales, under any setting. The case that reads as a bug.}
#'   \item{14. A confirmed ovulation but no onset ever recorded}{The
#'     menses-to-menses counter never starts, so there is no cycle length --
#'     and that one day still scales, pinned at `cyclic_time = 1`, because the
#'     ovulation anchor is set independently of any cycle. With
#'     `impute_next_menses = TRUE` a closing onset is imputed and the whole
#'     luteal phase scales, 15 days, in the confirmed columns too.}
#'   \item{15. Both opt-in rules at once, at opposite ends}{Ten rated days
#'     before the first onset \emph{and} a confirmed ovulation at the end with
#'     no closing onset. The only person who exercises both rules together, and
#'     therefore the only one where the order they are applied in could matter.
#'     It does not: 15 days at the front, 14 at the back, 29 together, with
#'     rows fabricated at both ends.}
#' }
#'
#' @section Overlaps, and where the order of operations matters:
#' `pacts_scaling()` applies `impute_next_menses` \strong{first} and
#' `impute_leading_ovulation` \strong{second}, so the leading rule can see a
#' table that already contains an imputed onset. Four things follow, all
#' measured on this dataset rather than reasoned from the code:
#' \itemize{
#'   \item \strong{The two rules do not interact.} They act on opposite ends of
#'     a diary, and on person 15, who has both, the days gained together are
#'     exactly the sum of the days each gains alone. The order is therefore not
#'     something a caller has to think about.
#'   \item \strong{An imputed onset never becomes a leading-day anchor}, and
#'     this holds structurally rather than by luck. `impute_next_menses` only
#'     ever imputes an onset forward from a confirmed ovulation, so whenever
#'     that imputed onset is a person's \emph{first} -- person 14, who has no
#'     observed onset at all -- there is by construction a confirmed ovulation
#'     before it, which is precisely the condition on which
#'     `impute_leading_ovulation` declines. No combination of the two settings
#'     imputes an ovulation before an imputed onset.
#'   \item \strong{`leading_ovulation_luteal_days` is ungated in both
#'     directions}, which person 3 demonstrates: nothing bounds how many
#'     leading days it will scale except the number you pass. Treat a value
#'     above the default as a methods decision to record, not a recovery
#'     setting to turn up.
#'   \item \strong{Persons 8 and 9 depend on a bound they do not demonstrate.}
#'     The phase-cap fallback that recovers them is itself gated on the cycle
#'     falling inside `[lower_cyclength_bound, upper_cyclength_bound]`, and both
#'     their cycles are 35 days -- the default upper bound exactly. At
#'     `upper_cyclength_bound = 34` their recovered days drop from 72 to 24 and
#'     the extended-phase flag from 47 to 0, so they stop demonstrating the
#'     phase caps at all. The CLEAR lab's own `[20, 43]` window keeps them
#'     working; a narrower one silently changes what they show.
#' }
#' Three overlaps are deliberate and not conflicts. Persons 5 and 14 both
#' exercise `impute_next_menses`, but on 5 the imputed onset closes a cycle that
#' already had an opening onset, while on 14 it creates the only onset that
#' person has. Person 12 is what the whole dataset looks like under
#' `ovtoday = NULL`, which is worth having in one person as well as one call.
#' And person 10 exists to contradict persons 8 and 9: the phase-length floors
#' and ceilings are not symmetric, and only the ceilings have a fallback.
#'
#' @source Generated, not real and not derived from any real record: the
#'   anchors are written out by hand and the symptom values come from a fixed
#'   random seed. The script that builds it, with the arithmetic for every case
#'   asserted as it runs, is `data-raw/cycledata_special.R` in the package
#'   sources; `tests/testthat/test_cycledata_special.R` checks that each case
#'   still behaves as its label says, so a change to a default that stops a
#'   person demonstrating their setting fails a test instead of passing
#'   silently.
#'
#' @note This dataset, its documentation and its automatic checks were drafted with
#'   Claude Code and reviewed by the package authors, who are responsible for their
#'   content. See the README for the package's full statement on AI tool use.
#'
#' @seealso [cycledata] for the ordinary practice dataset, [pacts_scaling()]
#'   for the settings each person exercises.
#'
#' @examples
#' # What each person is, and how much of them scales at the defaults
#' scaled <- pacts_scaling(
#'   cycledata_special,
#'   id = id, date = daterated, menses = menses, ovtoday = ovtoday
#' )
#'
#' # Two settings, and the days each one gains
#' leading <- pacts_scaling(
#'   cycledata_special,
#'   id = id, date = daterated, menses = menses, ovtoday = ovtoday,
#'   impute_leading_ovulation = TRUE
#' )
#'
#' # People 2, 3 and 15 gain 15 days each; nobody else changes
#' gained <- function(x) tapply(!is.na(x$cyclic_time_impute), x$id, sum)
#' gained(leading) - gained(scaled)
#'
#' # And the rows the package added, which carry no rating
#' sum(is.na(leading$symptom)) - sum(is.na(scaled$symptom))
"cycledata_special"
