# data-raw/cycledata_special.R ------------------------------------------------
# Builds data/cycledata_special.rda: the "special cases" practice dataset.
#
# WHY THIS EXISTS. The bundled `cycledata` is an ordinary dataset: 25 people,
# every cycle in range, no one observed before their first recorded menses, no
# right-censored luteal phase, no calendar gaps. So it cannot demonstrate -- or
# falsify a claim about -- any of the settings pacts_scaling() now has. Every
# person below exists to exercise exactly one of them, named in the `case`
# column, so an example can show the setting changing the answer.
#
# PROVENANCE. Entirely GENERATED, not real and not derived from any real
# record: the anchors below are written out by hand and the symptom values come
# from set.seed(2026) plus the formula at the bottom of this file. No
# participant's data is involved. Rerunning this script reproduces the dataset
# byte for byte.
#
# Run from the package root:  Rscript --vanilla data-raw/cycledata_special.R

suppressPackageStartupMessages(library(dplyr))

# One person's diary, built from its anchors rather than from literal dates, so
# the phase lengths each case claims are arithmetic and can be asserted below.
person <- function(id, case, first, last, onsets, ovs, drop = NULL) {
  d <- seq(first, last, by = "day")
  if (!is.null(drop)) d <- d[!d %in% drop]
  data.frame(id = id, daterated = d,
             menses  = as.integer(d %in% onsets),
             ovtoday = as.integer(d %in% ovs),
             case    = case, stringsAsFactors = FALSE)
}
# onsets from a first onset and a run of cycle lengths; ovulation at
# `fol` days after the onset of its own cycle (NA = not confirmed that cycle).
onsets_from <- function(first_onset, lengths) first_onset + c(0, cumsum(lengths))
ovs_from    <- function(first_onset, lengths, fol) {
  on <- onsets_from(first_onset, lengths)[seq_along(lengths)]
  stats::na.omit(on + fol)
}

A <- as.Date("2025-01-06")   # person 1's first onset; later people start later
s <- function(k) A + 17 * (k - 1)

people <- list()

## 1. Ordinary. Three complete cycles, ovulation confirmed, every phase in
##    range. The baseline: without it you cannot tell "this setting recovered
##    nothing" from "this dataset is broken".
{
  a <- s(1); L <- c(28, 29, 27); f <- c(14, 15, 13)
  people[[1]] <- person(1, "ordinary: three complete cycles, ovulation confirmed",
                        first = a, last = max(onsets_from(a, L)) + 5,
                        onsets = onsets_from(a, L), ovs = ovs_from(a, L, f))
}

## 2. Eight rated days BEFORE the first recorded menses onset, so the imputed
##    leading ovulation (15 days back) falls 7 days before the diary starts.
##    Exercises impute_leading_ovulation = TRUE *and* shows it fabricating
##    calendar rows: 7 of the 15 window days were never rated.
{
  a <- s(2); L <- c(28, 28); f <- c(14, 14)
  people[[2]] <- person(2, "8 rated days before the first recorded menses onset",
                        first = a - 8, last = max(onsets_from(a, L)) + 4,
                        onsets = onsets_from(a, L), ovs = ovs_from(a, L, f))
}

## 3. Thirty rated days before the first recorded onset. Shows that the 15 is
##    a PLACEMENT, not a cap: leading_ovulation_luteal_days = 25 scales 25 of
##    the 30 days, = 40 scales 40 and fabricates 10 rows beyond the diary, and
##    the luteal phase caps never gate any of it. Measured, not assumed.
{
  a <- s(3); L <- c(28, 30); f <- c(14, 15)
  people[[3]] <- person(3, "30 rated days before the first onset: how many scale is a setting, not a cap",
                        first = a - 30, last = max(onsets_from(a, L)) + 4,
                        onsets = onsets_from(a, L), ovs = ovs_from(a, L, f))
}

## 4. A CONFIRMED ovulation among the leading days, 14 days before the first
##    onset. Nothing is imputed for this person (the gate declines), and the
##    leading luteal days scale anyway, in the confirmed columns, with no
##    setting switched on. The 6 days before that ovulation have no onset on
##    their left, so no follicular fraction exists for them.
{
  a <- s(4); L <- c(28); f <- c(14)
  people[[4]] <- person(4, "confirmed ovulation among the leading days: scales with no setting",
                        first = a - 20, last = max(onsets_from(a, L)) + 4,
                        onsets = onsets_from(a, L), ovs = c(a - 14, ovs_from(a, L, f)))
}

## 5. A confirmed ovulation at the END of the diary with no closing menses
##    onset ever recorded. Exercises impute_next_menses = TRUE, which closes
##    the cycle 14 days after that ovulation -- past the last rated day, so it
##    fabricates that row too.
{
  a <- s(5); L <- c(28, 28); f <- c(14, 14)
  on <- onsets_from(a, L)
  people[[5]] <- person(5, "confirmed ovulation at the end, no closing menses onset",
                        first = a, last = max(on) + 24,
                        onsets = on, ovs = c(ovs_from(a, L, f), max(on) + 15))
}

## 6. An 18-day cycle with ovulation NOT confirmed. Shorter than
##    lower_cyclength_bound (21), so ovulation is not imputed for it and the
##    cycle has no cycle time in any column -- while the 28-day cycles on
##    either side of it do.
{
  a <- s(6); L <- c(28, 18, 28); f <- c(14, NA, 14)
  people[[6]] <- person(6, "an 18-day cycle, ovulation not confirmed: under lower_cyclength_bound",
                        first = a, last = max(onsets_from(a, L)) + 4,
                        onsets = onsets_from(a, L), ovs = ovs_from(a, L, f))
}

## 7. A 42-day cycle with ovulation NOT confirmed: longer than
##    upper_cyclength_bound (35), so no imputation. 42 is inside the CLEAR lab's
##    own [20,43] standard, which is why the lab widens these two arguments.
{
  a <- s(7); L <- c(28, 42, 28); f <- c(14, NA, 14)
  people[[7]] <- person(7, "a 42-day cycle, ovulation not confirmed: over upper_cyclength_bound",
                        first = a, last = max(onsets_from(a, L)) + 4,
                        onsets = onsets_from(a, L), ovs = ovs_from(a, L, f))
}

## 8. Confirmed ovulation 22 days before the next onset, so luteal_length
##    would read 21 -- past luteal_phase_max_days (18) -- inside a 35-day
##    cycle still within 21-35. cyclic_time leaves those days out;
##    cyclic_time_impute recovers them through the phase-cap fallback and
##    flags them. Labels below count days in the DIARY (onset to ovulation,
##    ovulation to next onset), because luteal_length counts the days strictly
##    between the two and so reads one less -- and reads NA here anyway,
##    suppressed by the very cap this person crosses.
{
  a <- s(8); L <- c(35); f <- c(13)
  people[[8]] <- person(8, "confirmed ovulation 22 days before the next onset: past luteal_phase_max_days",
                        first = a, last = max(onsets_from(a, L)) + 3,
                        onsets = onsets_from(a, L), ovs = ovs_from(a, L, f))
}

## 9. Confirmed ovulation 26 days AFTER the onset -- past
##    follicular_phase_max_days (25) -- inside a 35-day cycle, leaving a
##    9-day luteal phase that clears its own floor. Same fallback, other phase.
{
  a <- s(9); L <- c(35); f <- c(26)
  people[[9]] <- person(9, "confirmed ovulation 26 days after the onset: past follicular_phase_max_days",
                        first = a, last = max(onsets_from(a, L)) + 3,
                        onsets = onsets_from(a, L), ovs = ovs_from(a, L, f))
}

## 10. Confirmed ovulation whose luteal phase runs only 5 days -- under
##     luteal_phase_min_days (7). The contrast with people 8 and 9: the
##     fallback ignores the phase CEILINGS but still applies the FLOORS, so
##     these days are uncovered in every column, recoverable by nothing.
{
  a <- s(10); L <- c(26); f <- c(21)
  people[[10]] <- person(10, "confirmed ovulation 5 days before the next onset: under luteal_phase_min_days",
                         first = a, last = max(onsets_from(a, L)) + 3,
                         onsets = onsets_from(a, L), ovs = ovs_from(a, L, f))
}

## 11. Eleven days with no rows at all, mid-follicular, swallowing neither an
##     onset nor an ovulation. pacts_scaling() fills the calendar, so those 11
##     days come BACK in the output with no symptom on them: any coverage or
##     recovery figure has to be counted over rows with an observed outcome.
{
  a <- s(11); L <- c(28, 28, 28); f <- c(14, 14, 14)
  people[[11]] <- person(11, "an 11-day hole in the diary: rows come back with no rating",
                         first = a, last = max(onsets_from(a, L)) + 4,
                         onsets = onsets_from(a, L), ovs = ovs_from(a, L, f),
                         drop = seq(a + 30, a + 40, by = "day"))
}

## 12. Ovulation confirmed on no day at all, every cycle in range. The whole
##     record depends on imputed ovulation: cyclic_time is empty for this
##     person and cyclic_time_impute is complete. The confirmed-versus-imputed
##     contrast, in one person.
{
  a <- s(12); L <- c(28, 30, 27)
  people[[12]] <- person(12, "ovulation confirmed on no day: imputed columns only",
                         first = a, last = max(onsets_from(a, L)) + 5,
                         onsets = onsets_from(a, L), ovs = as.Date(character(0)))
}

## 13. No anchor of any kind: no menses onset and no confirmed ovulation, just
##     45 days of ratings. Nothing scales, under any setting. Included because
##     this is the case that reads as a bug to someone seeing empty columns.
{
  a <- s(13)
  people[[13]] <- person(13, "no menses onset and no confirmed ovulation: nothing can scale",
                         first = a, last = a + 44,
                         onsets = as.Date(character(0)), ovs = as.Date(character(0)))
}

## 14. A confirmed ovulation but no menses onset EVER recorded. The
##     menses-to-menses counter never starts, so there is no cycle length --
##     yet that one day still scales, pinned at cyclic_time = +1, because the
##     ovulation anchor is set independently of any cycle. Switch on
##     impute_next_menses and a closing onset is imputed 14 days later, which
##     scales the whole luteal phase in the CONFIRMED columns, not just the
##     imputed ones. Found by running the dataset, not by reading the code.
{
  a <- s(14)
  people[[14]] <- person(14, "confirmed ovulation but no onset ever: one day scales, or a whole luteal phase",
                         first = a, last = a + 44,
                         onsets = as.Date(character(0)), ovs = a + 20)
}

## 15. BOTH opt-in rules on one person, at opposite ends: 10 rated days before
##     the first onset AND a confirmed ovulation at the end with no closing
##     onset. pacts_scaling() applies impute_next_menses FIRST and
##     impute_leading_ovulation second, so this person is where that order
##     could matter. It does not: the gains are additive (15 days at the front,
##     14 at the back) and rows are fabricated at both ends. Nobody else in the
##     dataset exercises the two rules together, which is why this person
##     exists.
{
  a <- s(15); L <- c(28); f <- c(14)
  on <- onsets_from(a, L)
  people[[15]] <- person(15, "both opt-in rules at once, at opposite ends of one diary",
                         first = a - 10, last = max(on) + 24,
                         onsets = on, ovs = c(ovs_from(a, L, f), max(on) + 15))
}

cycledata_special <- dplyr::bind_rows(people) %>% dplyr::arrange(.data$id, .data$daterated)

# --- symptom: a perimenstrual peak plus noise, ~8% missing at random --------
set.seed(2026)
near <- vapply(seq_len(nrow(cycledata_special)), function(i) {
  on <- cycledata_special$daterated[cycledata_special$id == cycledata_special$id[i] &
                                    cycledata_special$menses == 1]
  if (!length(on)) return(NA_real_)
  min(abs(as.numeric(cycledata_special$daterated[i] - on)))
}, numeric(1))
sig <- 2.5 + 3 * exp(-(ifelse(is.na(near), 7, near) / 4)^2)
sym <- round(pmin(pmax(sig + stats::rnorm(nrow(cycledata_special), 0, 1.1), 0), 10))
sym[stats::runif(nrow(cycledata_special)) < 0.08] <- NA_real_
cycledata_special$symptom <- sym

cycledata_special <- cycledata_special[, c("id", "daterated", "symptom", "menses", "ovtoday", "case")]
cycledata_special$id <- as.integer(cycledata_special$id)

# --- the design asserts itself ---------------------------------------------
# Each claim in a `case` label is arithmetic on the anchors, so check it here
# rather than trusting the comments. A failure means the spec above is wrong.
chk <- function(cond, what) if (!isTRUE(cond)) stop("design check failed: ", what, call. = FALSE)
g <- function(k) dplyr::filter(cycledata_special, .data$id == k)
lead <- function(k) { p <- g(k); sum(p$daterated < min(p$daterated[p$menses == 1])) }
chk(lead(2) == 8,  "person 2 has 8 leading days")
chk(lead(3) == 30, "person 3 has 30 leading days")
chk(lead(4) == 20, "person 4 has 20 leading days")
chk(lead(15) == 10, "person 15 has 10 leading days")
chk(sum(g(15)$menses) == 2 && sum(g(15)$ovtoday) == 2,
    "person 15 has two onsets and two confirmed ovulations, the second unclosed")
chk(all(diff(g(1)$daterated) == 1), "person 1 has no calendar holes")
chk(sum(diff(g(11)$daterated) == 12) == 1, "person 11 has exactly one 11-day hole")
chk(nrow(g(13)) == 45 && sum(g(13)$menses) == 0 && sum(g(13)$ovtoday) == 0,
    "person 13 has no anchor of any kind")
chk(nrow(g(14)) == 45 && sum(g(14)$menses) == 0 && sum(g(14)$ovtoday) == 1,
    "person 14 has one confirmed ovulation and no onset")
chk(sum(g(12)$ovtoday) == 0, "person 12 has no confirmed ovulation")
# phase lengths for the four phase-cap people
phases <- function(k) {
  p <- g(k); on <- p$daterated[p$menses == 1]; ov <- p$daterated[p$ovtoday == 1]
  ov <- ov[ov > min(on) & ov < max(on)]
  c(fol = as.numeric(ov[1] - max(on[on < ov[1]])),
    lut = as.numeric(min(on[on > ov[1]]) - ov[1]))
}
chk(all(phases(8)  == c(13, 22)), "person 8: onset-to-ovulation 13, ovulation-to-onset 22")
chk(all(phases(9)  == c(26, 9)),  "person 9: onset-to-ovulation 26, ovulation-to-onset 9")
chk(all(phases(10) == c(21, 5)),  "person 10: onset-to-ovulation 21, ovulation-to-onset 5")
chk(all(phases(1)  == c(14, 14)), "person 1: follicular 14, luteal 14")
cyclen <- function(k) { p <- g(k); as.numeric(diff(p$daterated[p$menses == 1])) }
chk(identical(cyclen(6), c(28, 18, 28)), "person 6 cycles are 28/18/28")
chk(identical(cyclen(7), c(28, 42, 28)), "person 7 cycles are 28/42/28")
chk(identical(cyclen(12), c(28, 30, 27)), "person 12 cycles are 28/30/27")

cat(sprintf("cycledata_special: %d rows, %d people, %d rated days\n",
            nrow(cycledata_special), dplyr::n_distinct(cycledata_special$id),
            sum(!is.na(cycledata_special$symptom))))
print(cycledata_special %>% dplyr::group_by(.data$id, .data$case) %>%
        dplyr::summarise(days = dplyr::n(), onsets = sum(.data$menses),
                         confirmed_ov = sum(.data$ovtoday), .groups = "drop") %>%
        as.data.frame(), right = FALSE)

save(cycledata_special, file = "data/cycledata_special.rda",
     compress = "bzip2", version = 2)
