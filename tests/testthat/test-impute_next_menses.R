# Tests for the opt-in next-menses imputation (impute_next_menses).
# The load-bearing guarantee: the default (FALSE) changes NOTHING for existing
# users; the opt-in applies the documented general rule.

test_that("impute_next_menses defaults are inert (published behavior unchanged)", {
  a <- pacts_scaling(cycledata, id = id, date = daterated, menses = menses, ovtoday = ovtoday)
  b <- pacts_scaling(cycledata, id = id, date = daterated, menses = menses, ovtoday = ovtoday,
                     impute_next_menses = FALSE)
  expect_equal(a, b)
  expect_false("menses_impute" %in% names(a))   # no new column when off
})

test_that("impute_next_menses = TRUE imputes ov+14 for an open ovulation, skips closed cycles", {
  mk <- function(id, ov_day, mens_days, n = 40) data.frame(
    id        = id,
    daterated = as.Date("2026-01-01") + 0:(n - 1),
    menses    = as.integer((0:(n - 1)) %in% (mens_days - 1)),
    ovtoday   = as.integer((0:(n - 1)) == (ov_day - 1)))
  synth <- rbind(mk("A", ov_day = 15, mens_days = c(1)),        # open ovulation -> impute at day 29
                 mk("B", ov_day = 15, mens_days = c(1, 25)))    # menses day 25 closes it -> no impute
  imp <- impute_next_menses_onsets(synth, id, daterated, menses, ovtoday)

  expect_equal(as.character(imp$daterated[imp$id == "A" & imp$menses_impute == 1]), "2026-01-29")
  expect_equal(sum(imp$menses_impute[imp$id == "B"]), 0)
})

test_that("custom luteal-days / window are respected", {
  mk <- function(id, ov_day, mens_days, n = 40) data.frame(
    id = id, daterated = as.Date("2026-01-01") + 0:(n - 1),
    menses = as.integer((0:(n - 1)) %in% (mens_days - 1)),
    ovtoday = as.integer((0:(n - 1)) == (ov_day - 1)))
  synth <- mk("A", ov_day = 15, mens_days = c(1))
  imp <- impute_next_menses_onsets(synth, id, daterated, menses, ovtoday, luteal_days = 13)
  expect_equal(as.character(imp$daterated[imp$menses_impute == 1]), "2026-01-28")  # day 15 + 13
})

test_that("impute_next_menses = TRUE increases scaled coverage on cycledata", {
  off <- pacts_scaling(cycledata, id = id, date = daterated, menses = menses, ovtoday = ovtoday)
  on  <- pacts_scaling(cycledata, id = id, date = daterated, menses = menses, ovtoday = ovtoday,
                       impute_next_menses = TRUE)
  expect_gt(sum(!is.na(on$cyclic_time_impute)), sum(!is.na(off$cyclic_time_impute)))
  expect_true("menses_impute" %in% names(on))
})
