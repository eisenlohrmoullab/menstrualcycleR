# RELEASE-BLOCKER FIX (pre-release adversarial review, 2026-08-20, finding R1): a right-censored
# luteal phase -- a confirmed ovulation with no closing menses yet observed, because data
# collection simply ended -- was being scaled as if it were a complete, closed luteal phase, in
# the STRICT confirmed-only columns (cyclic_time, cyclic_time_ov, luteal_length), completely
# unflagged (cyclic_time_impute_extended_phase stayed 0). The run-closing loop in
# process_luteal_phase_base() (R/helper.R) could not distinguish "this run closed because a real
# menses onset was observed" from "this run closed because the data (or this id's rows) simply
# ran out" -- both leave the next row's lutdaycount NA. Present in every prior release (the bug
# predates 0.1.7). Real exposure in the shipped CLEAR v20260819 release, measured carefully against
# anchor snapshots (an initial "20 participants / 112 days" estimate, measured against release rows,
# turned out to be a large overcount -- most censored tails were already covered by the pipeline's
# own flagged rescue policy, which is separate from this bug): 4 unflagged fabricated confirmed
# days (participant 3140) plus 14 treatment-phase day-slots in a derived layer. See
# pacts-gam-pipeline/cycle_data_prep/CYCLE_METHODS_DECISIONS.md D-2026-08-20c for the full
# bookkeeping. Fixed by requiring the run's terminal row to actually be a menses onset before
# closing lutmax.

test_that("BUG FIX (R1): a right-censored luteal phase (no closing menses, data just ends) is NOT scaled -- cyclic_time, cyclic_time_ov, and luteal_length all stay NA, cycle_incomplete stays 1", {
  # menses day 1, confirmed ov day 15, data ends 10 days later with NO closing menses.
  n <- 25
  df <- data.frame(id = "A", date = as.Date("2026-01-01") + 0:(n - 1),
                    menses = as.integer((0:(n - 1)) == 0),
                    ovtoday = as.integer((0:(n - 1)) == 14))
  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)

  luteal_rows <- out$date > as.Date("2026-01-15")
  expect_true(all(out$cycle_incomplete[luteal_rows] == 1))
  expect_true(all(is.na(out$cyclic_time[luteal_rows])))
  expect_true(all(is.na(out$cyclic_time_ov[luteal_rows])))
  expect_true(all(is.na(out$scaled_cycleday[luteal_rows])))
  expect_true(all(is.na(out$scaled_cycleday_ov[luteal_rows])))
  expect_true(all(is.na(out$luteal_length[luteal_rows])))
  # NOT flagged as fallback-derived either -- it's genuinely NA, not filled via
  # the uncapped tier (that tier is separately, correctly gated on
  # cycle_incomplete != 1 -- see test_cyclic_time_impute_extended_phase.R).
  expect_true(all(out$cyclic_time_impute_extended_phase[luteal_rows] == 0))
})

test_that("a right-censored luteal phase at a MID-dataset participant boundary is also NOT scaled (not just the last participant in the file)", {
  n <- 25
  a <- data.frame(id = "A", date = as.Date("2026-01-01") + 0:(n - 1),
                   menses = as.integer((0:(n - 1)) == 0), ovtoday = as.integer((0:(n - 1)) == 14))
  # id B comes AFTER A in the file, and has its OWN complete, closed cycle --
  # A's right-censoring must not be masked by B's rows existing afterward.
  b <- data.frame(id = "B", date = as.Date("2026-01-01") + 0:34,
                   menses = as.integer((0:34) %in% c(0, 30)), ovtoday = as.integer((0:34) == 14))
  df <- rbind(a, b)
  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)

  a_luteal <- out$id == "A" & out$date > as.Date("2026-01-15")
  expect_true(all(is.na(out$cyclic_time[a_luteal])))
  expect_true(all(is.na(out$luteal_length[a_luteal])))

  # B's complete cycle is completely unaffected.
  b_luteal <- out$id == "B" & out$date > as.Date("2026-01-15") & out$date <= as.Date("2026-01-31")
  expect_true(all(!is.na(out$cyclic_time[b_luteal])))
  expect_equal(unique(na.omit(out$luteal_length[out$id == "B"])), 15)
})

test_that("the deliberately-supported ov...menses fragment (no opening menses observed) still scales correctly", {
  # data starts mid-cycle: ov day 1 (no preceding observed menses), closing
  # menses day 15 -- this fragment must still close normally; R1's fix must
  # not accidentally require an OPENING menses too.
  df <- data.frame(id = "C", date = as.Date("2026-01-01") + 0:14,
                    menses = as.integer((0:14) == 14), ovtoday = as.integer((0:14) == 0))
  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)
  expect_true(all(!is.na(out$cyclic_time)))
  expect_equal(out$cyclic_time[1], 1)
  expect_equal(out$cyclic_time[nrow(out)], 0)
})

test_that("an ordinary complete, closed luteal phase is completely unaffected by the R1 fix", {
  df <- data.frame(id = "D", date = as.Date("2026-01-01") + 0:34,
                    menses = as.integer((0:34) %in% c(0, 30)), ovtoday = as.integer((0:34) == 14))
  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)
  luteal_rows <- out$date > as.Date("2026-01-15") & out$date <= as.Date("2026-01-31")
  expect_true(all(!is.na(out$cyclic_time[luteal_rows])))
  expect_equal(unique(na.omit(out$luteal_length)), 15)
})

test_that("a right-censored luteal phase whose fabricated coverage would otherwise land inside the caller's bounds by coincidence is still excluded", {
  # mcyclength for a still-open trailing run is just "however many days were
  # observed so far" -- this can land inside [lower,upper] purely by when
  # the file happens to end. That coincidence must not be enough to close it.
  n <- 30  # ov day 15, 15 more observed days, mcyclength-so-far = 29 (inside [21,35])
  df <- data.frame(id = "E", date = as.Date("2026-01-01") + 0:(n - 1),
                    menses = as.integer((0:(n - 1)) == 0), ovtoday = as.integer((0:(n - 1)) == 14))
  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)
  luteal_rows <- out$date > as.Date("2026-01-15")
  expect_true(all(out$mcyclength[luteal_rows] >= 21 & out$mcyclength[luteal_rows] <= 35))
  expect_true(all(is.na(out$cyclic_time[luteal_rows])))
})
