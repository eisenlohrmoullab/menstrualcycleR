# Tests for luteal_phase_min_days/max_days and follicular_phase_min_days/max_days
# -- the four arguments that make the previously-hardcoded 18d/25d phase caps
# adjustable. Defaults (7, 18, 8, 25) must reproduce prior behavior exactly;
# widening or narrowing them must move cyclic_time (not just cyclic_time_impute).

mk_cycle <- function(id, menses_days, ov_day, n) {
  data.frame(
    id      = id,
    date    = as.Date("2026-01-01") + 0:(n - 1),
    menses  = as.integer((0:(n - 1)) %in% (menses_days - 1)),
    ovtoday = as.integer((0:(n - 1)) == (ov_day - 1))
  )
}

test_that("default phase bounds (7/18/8/25) reproduce the hardcoded pre-configurable behavior exactly", {
  explicit <- pacts_scaling(cycledata, id = id, date = daterated, menses = menses, ovtoday = ovtoday,
                             luteal_phase_min_days = 7, luteal_phase_max_days = 18,
                             follicular_phase_min_days = 8, follicular_phase_max_days = 25)
  implicit <- pacts_scaling(cycledata, id = id, date = daterated, menses = menses, ovtoday = ovtoday)
  expect_equal(explicit, implicit)
})

test_that("widening luteal_phase_max_days lets cyclic_time itself cover a previously over-cap luteal phase", {
  # menses day 1, ovulation day 21 (follicular 20, in-band), next menses day 61
  # (luteal 40 days -- over the default 18d cap, but within a widened 45d cap).
  df <- mk_cycle("A", menses_days = c(1, 61), ov_day = 21, n = 65)

  default_bounds <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                                   lower_cyclength_bound = 20, upper_cyclength_bound = 65)
  widened <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                            lower_cyclength_bound = 20, upper_cyclength_bound = 65,
                            luteal_phase_max_days = 45)

  luteal_rows <- df$id == "A" & df$date > as.Date("2026-01-21") & df$date < as.Date("2026-03-02")

  # under defaults, cyclic_time is NA for the over-cap luteal run (as tested
  # elsewhere) -- confirm that still holds here as the baseline.
  expect_true(all(is.na(default_bounds$cyclic_time[luteal_rows])))

  # widened: cyclic_time itself now covers those days directly -- no fallback
  # needed, this is the actual confirmed-ovulation column moving.
  expect_true(all(!is.na(widened$cyclic_time[luteal_rows])))
  # and since cyclic_time is now non-NA, the extended-phase fallback never
  # fires for these rows -- they're covered by the real thing, not a stand-in.
  expect_true(all(widened$cyclic_time_impute_extended_phase[luteal_rows] == 0))
  expect_equal(widened$cyclic_time_impute[luteal_rows], widened$cyclic_time[luteal_rows])
})

test_that("widening follicular_phase_max_days lets cyclic_time itself cover a previously over-cap follicular phase", {
  # menses day 1, ovulation day 40 (follicular 39 days -- over the default 25d
  # cap), next menses day 55 (luteal 15 days, in-band).
  df <- mk_cycle("A", menses_days = c(1, 55), ov_day = 40, n = 60)

  default_bounds <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                                   lower_cyclength_bound = 20, upper_cyclength_bound = 60)
  widened <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                            lower_cyclength_bound = 20, upper_cyclength_bound = 60,
                            follicular_phase_max_days = 45)

  follicular_rows <- df$id == "A" & df$date >= as.Date("2026-01-01") & df$date < as.Date("2026-02-09")

  expect_true(all(is.na(default_bounds$cyclic_time[follicular_rows])))
  expect_true(all(!is.na(widened$cyclic_time[follicular_rows])))
  expect_true(all(widened$cyclic_time_impute_extended_phase[follicular_rows] == 0))
})

test_that("the extended-phase fallback still applies beyond a caller-WIDENED phase bound", {
  # Same 40-day luteal phase as above, but only widen luteal_phase_max_days to
  # 30 -- still short of the real 40-day run. cyclic_time should stay NA
  # (30 < 40), but cyclic_time_impute should still cover it via the fallback,
  # correctly flagged.
  df <- mk_cycle("A", menses_days = c(1, 61), ov_day = 21, n = 65)

  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                        lower_cyclength_bound = 20, upper_cyclength_bound = 65,
                        luteal_phase_max_days = 30)

  luteal_rows <- df$id == "A" & df$date > as.Date("2026-01-21") & df$date < as.Date("2026-03-02")
  expect_true(all(is.na(out$cyclic_time[luteal_rows])))
  expect_true(all(!is.na(out$cyclic_time_impute[luteal_rows])))
  expect_true(all(out$cyclic_time_impute_extended_phase[luteal_rows] == 1))
})

test_that("narrowing luteal_phase_max_days excludes a phase that defaults would have included", {
  # menses day 1, ovulation day 15 (follicular 14, in-band), next menses day
  # 31 (luteal 16 days -- in-band under the default 18d cap, out-of-band
  # under a narrowed 10d cap).
  df <- mk_cycle("A", menses_days = c(1, 31), ov_day = 15, n = 35)

  default_bounds <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                                   lower_cyclength_bound = 20, upper_cyclength_bound = 43)
  narrowed <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                             lower_cyclength_bound = 20, upper_cyclength_bound = 43,
                             luteal_phase_max_days = 10)

  luteal_rows <- df$id == "A" & df$date > as.Date("2026-01-15") & df$date < as.Date("2026-01-31")
  expect_true(any(luteal_rows))
  expect_true(all(!is.na(default_bounds$cyclic_time[luteal_rows])))
  expect_true(all(is.na(narrowed$cyclic_time[luteal_rows])))
})

test_that("luteal_length respects the configurable bounds too", {
  df <- mk_cycle("A", menses_days = c(1, 61), ov_day = 21, n = 65)
  widened <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                            lower_cyclength_bound = 20, upper_cyclength_bound = 65,
                            luteal_phase_max_days = 45)
  luteal_rows <- df$id == "A" & df$date > as.Date("2026-01-21") & df$date < as.Date("2026-03-02")
  expect_true(all(!is.na(widened$luteal_length[luteal_rows])))
  # 39, not 40: luteal_length/lutmax is a 0-indexed day count from ovulation
  # (day 0) through the day before next menses, not a calendar day span.
  expect_true(all(widened$luteal_length[luteal_rows] == 39))
})
