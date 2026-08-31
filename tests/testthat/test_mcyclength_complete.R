# mcyclength_complete: a poka-yoke companion to mcyclength, added after `clearcom_thc`
# (a lab downstream consumer) shipped an analysis that filtered directly on `mcyclength`
# without also requiring `cycle_incomplete == 0`, silently admitting 263 person-days on a
# still-open trailing cycle's days-observed-so-far as if it were a real cycle length. The
# vignette already documented the correct two-clause filter, but a user reading only
# `?pacts_scaling` (mcyclength is not exported by any other function) would never see it.
# mcyclength_complete makes the mistake structurally impossible instead of relying on the
# caller remembering a second clause: NA there can never satisfy a numeric comparison.

test_that("mcyclength_complete equals mcyclength on complete cycles and is NA on incomplete ones", {
  cycle_df <- cycledata
  out <- pacts_scaling(cycle_df, id = id, date = daterated, menses = menses, ovtoday = ovtoday,
                        lower_cyclength_bound = 21, upper_cyclength_bound = 35)

  expect_true("mcyclength_complete" %in% names(out))
  expect_true(all(is.na(out$mcyclength_complete[out$cycle_incomplete == 1])))
  complete_rows <- !is.na(out$cycle_incomplete) & out$cycle_incomplete == 0
  expect_true(all(out$mcyclength_complete[complete_rows] == out$mcyclength[complete_rows]))
})

test_that("a right-censored cycle whose days-observed-so-far coincidentally falls inside the caller's bounds is excluded by mcyclength_complete but NOT by mcyclength alone", {
  # Same construction as the mcyclength coincidence case in
  # test_right_censored_luteal.R: ov day 15, 15 more observed days,
  # mcyclength-so-far = 29 -- inside [21, 35] purely because of when the file
  # happens to end, not because this is a real 29-day cycle.
  n <- 30
  df <- data.frame(id = "E", date = as.Date("2026-01-01") + 0:(n - 1),
                    menses = as.integer((0:(n - 1)) == 0), ovtoday = as.integer((0:(n - 1)) == 14))
  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)

  censored_rows <- out$date > as.Date("2026-01-15")
  expect_true(all(out$cycle_incomplete[censored_rows] == 1))

  # The old, unsafe idiom -- filtering on mcyclength alone -- wrongly admits these rows.
  unsafe <- dplyr::filter(out, mcyclength >= 21, mcyclength <= 35)
  expect_true(any(unsafe$date > as.Date("2026-01-15")))

  # mcyclength_complete excludes them without needing a separate cycle_incomplete clause.
  safe <- dplyr::filter(out, mcyclength_complete >= 21, mcyclength_complete <= 35)
  expect_false(any(safe$date > as.Date("2026-01-15")))
})

test_that("mcyclength_complete filtering reproduces the documented two-clause mcyclength/cycle_incomplete filter exactly", {
  cycle_df <- cycledata
  out <- pacts_scaling(cycle_df, id = id, date = daterated, menses = menses, ovtoday = ovtoday,
                        lower_cyclength_bound = 21, upper_cyclength_bound = 35)

  via_complete_col <- dplyr::filter(out, mcyclength_complete >= 21, mcyclength_complete <= 35)
  via_two_clauses <- dplyr::filter(out, cycle_incomplete == 0, mcyclength >= 21, mcyclength <= 35)
  expect_equal(nrow(via_complete_col), nrow(via_two_clauses))
  via_complete_col <- dplyr::arrange(via_complete_col, id, date)
  via_two_clauses <- dplyr::arrange(via_two_clauses, id, date)
  expect_equal(via_complete_col$id, via_two_clauses$id)
  expect_equal(via_complete_col$date, via_two_clauses$date)
})

test_that("mcyclength_complete is NA (not an error) on rows before a person's first menses onset, where cycle_incomplete is itself NA", {
  # cycle_incomplete is NA (not 0/1) on the pre-first-menses stretch; mcyclength_complete
  # must stay NA there too rather than erroring or coercing NA to some other value.
  df <- data.frame(id = "F", date = as.Date("2026-01-01") + 0:9,
                    menses = as.integer((0:9) == 5), ovtoday = rep(0L, 10))
  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)
  pre_menses <- out$date < as.Date("2026-01-06")
  expect_true(all(is.na(out$cycle_incomplete[pre_menses])))
  expect_true(all(is.na(out$mcyclength_complete[pre_menses])))
})
