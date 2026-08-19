# Tests for the cyclic_time_imp_ov phase-cap fallback -- the ovulation-centered
# sibling of cyclic_time_impute's fallback (see
# test_cyclic_time_impute_extended_phase.R for the full background).
#
# cyclic_lut_ov/cyclic_fol_ov (and therefore cyclic_time_ov) only scale a
# confirmed ovulation's luteal phase when it is 7-18 days, and its follicular
# phase when 8-25 days -- the same fixed internal caps, independent of
# whatever lower_cyclength_bound/upper_cyclength_bound the caller passes.
# cyclic_time_imp_ov now falls back one tier further, to the same phase math
# without the internal cap, gated on the cycle staying within the caller's
# own bounds -- mirroring cyclic_time_impute's fallback exactly, but centered
# on ovulation (ovtoday == 1 corresponds to 0, not menses). cyclic_time_ov
# itself must never be touched by this.

mk_cycle <- function(id, menses_days, ov_day, n) {
  data.frame(
    id      = id,
    date    = as.Date("2026-01-01") + 0:(n - 1),
    menses  = as.integer((0:(n - 1)) %in% (menses_days - 1)),
    ovtoday = as.integer((0:(n - 1)) == (ov_day - 1))
  )
}

test_that("a confirmed ovulation with an over-cap luteal phase gets cyclic_time_imp_ov coverage, not cyclic_time_ov", {
  # menses day 1, ovulation day 21 (follicular length 20 -- in-band, 8-25),
  # next menses day 61 (luteal length 40 -- over the 18-day internal cap).
  # Overall mcyclength = 60; use bounds wide enough to accept the whole
  # 60-day cycle so the fallback's own mcyclength gate is satisfied.
  df <- mk_cycle("A", menses_days = c(1, 61), ov_day = 21, n = 65)

  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                        lower_cyclength_bound = 20, upper_cyclength_bound = 65)

  luteal_rows <- out$id == "A" & out$date > as.Date("2026-01-21") & out$date < as.Date("2026-03-02")
  follicular_rows <- out$id == "A" & out$date >= as.Date("2026-01-01") & out$date < as.Date("2026-01-21")

  # cyclic_time_ov (confirmed, strict) stays NA for the over-cap luteal run --
  # completely unchanged behavior, this is the guarantee that must never break.
  expect_true(all(is.na(out$cyclic_time_ov[luteal_rows])))

  # cyclic_time_imp_ov now covers those same days via the extended-phase
  # fallback, and flags them.
  expect_true(all(!is.na(out$cyclic_time_imp_ov[luteal_rows])))
  expect_true(all(out$cyclic_time_imp_ov_extended_phase[luteal_rows] == 1))

  # the in-band follicular run needed no fallback at all -- confirmed path
  # covers it directly, flag stays 0.
  expect_true(all(!is.na(out$cyclic_time_ov[follicular_rows])))
  expect_true(all(out$cyclic_time_imp_ov_extended_phase[follicular_rows] == 0))
  expect_equal(out$cyclic_time_imp_ov[follicular_rows], out$cyclic_time_ov[follicular_rows])
})

test_that("the extended-phase fallback is gated on the caller's own upper_cyclength_bound -- it does not extend past what the caller asked for", {
  # Same shape as above, but scale with the CLEAR lab standard [20,43]: the
  # 60-day overall cycle is itself out of bounds, so the fallback must NOT
  # fire -- cyclic_time_imp_ov should stay NA for the capped-out luteal run,
  # same as cyclic_time_ov.
  df <- mk_cycle("A", menses_days = c(1, 61), ov_day = 21, n = 65)

  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                        lower_cyclength_bound = 20, upper_cyclength_bound = 43)

  luteal_rows <- out$id == "A" & out$date > as.Date("2026-01-21") & out$date < as.Date("2026-03-02")
  expect_true(all(is.na(out$cyclic_time_imp_ov[luteal_rows])))
  expect_true(all(out$cyclic_time_imp_ov_extended_phase[luteal_rows] == 0))
})

test_that("an ordinary in-cap cycle is completely unaffected -- cyclic_time_imp_ov equals cyclic_time_ov exactly", {
  # menses day 1, ovulation day 15 (follicular 14, in-band), next menses day
  # 31 (luteal 16, in-band). Nothing here should ever touch the new fallback.
  df <- mk_cycle("A", menses_days = c(1, 31), ov_day = 15, n = 35)

  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                        lower_cyclength_bound = 20, upper_cyclength_bound = 43)

  expect_true(all(out$cyclic_time_imp_ov_extended_phase == 0))
  rows_with_confirmed <- !is.na(out$cyclic_time_ov)
  expect_equal(out$cyclic_time_imp_ov[rows_with_confirmed], out$cyclic_time_ov[rows_with_confirmed])
})

test_that("cyclic_time_imp_ov_extended_phase is always present and 0/1 only, never NA", {
  df <- mk_cycle("A", menses_days = c(1, 61), ov_day = 21, n = 65)
  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                        lower_cyclength_bound = 20, upper_cyclength_bound = 65)
  expect_true("cyclic_time_imp_ov_extended_phase" %in% names(out))
  expect_false(any(is.na(out$cyclic_time_imp_ov_extended_phase)))
  expect_true(all(out$cyclic_time_imp_ov_extended_phase %in% c(0L, 1L)))
})

test_that("the ovulation-anchor day is never flagged extended_phase, even when its phase is over-cap", {
  # calculate_cycletime()'s own post-processing unconditionally pins
  # cyclic_time_imp_ov to 0 at ovtoday==1, regardless of which tier supplied
  # a value inside create_scaled_cycleday(). The flag must not claim that
  # pinned day as fallback-derived -- its final value never actually depends
  # on the extended-phase fallback.
  df <- mk_cycle("A", menses_days = c(1, 61), ov_day = 21, n = 65)
  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                        lower_cyclength_bound = 20, upper_cyclength_bound = 65)

  ov_row <- out$id == "A" & out$date == as.Date("2026-01-21")
  expect_equal(sum(ov_row), 1)
  expect_equal(out$cyclic_time_imp_ov_extended_phase[ov_row], 0L)
  # still correctly pinned to 0 by the anchor override -- the VALUE is right,
  # only the flag changed.
  expect_equal(out$cyclic_time_imp_ov[ov_row], 0)
})

test_that("an incomplete trailing cycle does not get extended-phase fallback coverage", {
  # A confirmed ovulation whose luteal run is still open at the end of the
  # file (no closing menses ever observed) is cycle_incomplete == 1.
  # mcyclength for it is just "however many days were observed", which can
  # land inside the caller's bounds by coincidence -- that must not be
  # enough to trigger the fallback. menses day 1, ovulation day 21
  # (follicular 20, in-band), data ends day 45 with NO closing menses.
  df <- mk_cycle("A", menses_days = 1, ov_day = 21, n = 45)

  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                        lower_cyclength_bound = 20, upper_cyclength_bound = 50)

  luteal_rows <- out$id == "A" & out$date > as.Date("2026-01-21")
  # sanity: this trailing cycle really is marked incomplete, and its
  # observed-so-far mcyclength really does land inside [20,50] -- otherwise
  # this test isn't exercising the gate it claims to.
  expect_true(all(out$cycle_incomplete[luteal_rows] == 1))
  expect_true(all(out$mcyclength[luteal_rows] >= 20 & out$mcyclength[luteal_rows] <= 50))
  expect_true(any(luteal_rows))
  expect_true(all(is.na(out$cyclic_time_imp_ov[luteal_rows])))
  expect_true(all(out$cyclic_time_imp_ov_extended_phase[luteal_rows] == 0))
})
