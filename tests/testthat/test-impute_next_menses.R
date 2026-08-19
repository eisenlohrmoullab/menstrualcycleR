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

test_that("custom luteal-days is respected", {
  mk <- function(id, ov_day, mens_days, n = 40) data.frame(
    id = id, daterated = as.Date("2026-01-01") + 0:(n - 1),
    menses = as.integer((0:(n - 1)) %in% (mens_days - 1)),
    ovtoday = as.integer((0:(n - 1)) == (ov_day - 1)))
  synth <- mk("A", ov_day = 15, mens_days = c(1))
  imp <- impute_next_menses_onsets(synth, id, daterated, menses, ovtoday, luteal_days = 13)
  expect_equal(as.character(imp$daterated[imp$menses_impute == 1]), "2026-01-28")  # day 15 + 13
})

test_that("BUG FIX (0.1.7): a real closing menses beyond the old 20-day window is never overwritten by a phantom onset", {
  # Reproduces the exact shape of the bug found on the package's own cycledata
  # (id 8): a real ~22-day luteal phase. Through 0.1.6, has_closing only
  # searched (ov, ov+max_window] -- a real closing 22 days out fell outside
  # the default 20-day window, so the function imputed a phantom onset at
  # ov+14 anyway, FLIPPING that actually-observed day's menses from 0 to 1.
  mk <- function(id, ov_day, mens_days, n = 60) data.frame(
    id = id, daterated = as.Date("2026-01-01") + 0:(n - 1),
    menses = as.integer((0:(n - 1)) %in% (mens_days - 1)),
    ovtoday = as.integer((0:(n - 1)) == (ov_day - 1)))
  # ov day 1, real closing menses day 23 (22 days later -- past the old
  # default max_window of 20).
  synth <- mk("A", ov_day = 1, mens_days = c(1, 23))

  imp <- impute_next_menses_onsets(synth, id, daterated, menses, ovtoday)

  # no phantom onset anywhere -- the real day-23 closing is respected.
  expect_equal(sum(imp$menses_impute), 0)
  # the would-be phantom date (ov + 14 = day 15) must still read its
  # ORIGINAL observed value (0), not be silently flipped to 1.
  phantom_date <- as.character(as.Date("2026-01-01") + 14)
  expect_equal(imp$menses[imp$daterated == phantom_date], 0)
  # the real closing onset is untouched.
  real_close <- as.character(as.Date("2026-01-01") + 22)
  expect_equal(imp$menses[imp$daterated == real_close], 1)
})

test_that("max_window no longer affects the result -- kept for signature stability only", {
  mk <- function(id, ov_day, mens_days, n = 60) data.frame(
    id = id, daterated = as.Date("2026-01-01") + 0:(n - 1),
    menses = as.integer((0:(n - 1)) %in% (mens_days - 1)),
    ovtoday = as.integer((0:(n - 1)) == (ov_day - 1)))
  synth <- mk("A", ov_day = 1, mens_days = c(1, 23))  # same 22-day real closing as above

  narrow  <- impute_next_menses_onsets(synth, id, daterated, menses, ovtoday, max_window = 5)
  wide    <- impute_next_menses_onsets(synth, id, daterated, menses, ovtoday, max_window = 1000)
  default <- impute_next_menses_onsets(synth, id, daterated, menses, ovtoday)
  expect_equal(narrow, wide)
  expect_equal(narrow, default)
})

test_that("a genuinely open cycle (no closing menses ever recorded) still imputes correctly under the unbounded search", {
  mk <- function(id, ov_day, mens_days, n = 40) data.frame(
    id = id, daterated = as.Date("2026-01-01") + 0:(n - 1),
    menses = as.integer((0:(n - 1)) %in% (mens_days - 1)),
    ovtoday = as.integer((0:(n - 1)) == (ov_day - 1)))
  synth <- mk("A", ov_day = 15, mens_days = c(1))  # no closing menses anywhere in 40 days
  imp <- impute_next_menses_onsets(synth, id, daterated, menses, ovtoday)
  expect_equal(as.character(imp$daterated[imp$menses_impute == 1]), "2026-01-29")  # day 15 + 14
})

test_that("impute_next_menses = TRUE increases CONFIRMED scaled coverage for a genuinely open cycle", {
  # cyclic_time (confirmed-ovulation only) is the column impute_next_menses
  # directly targets -- closing an open confirmed-ovulation cycle lets its
  # luteal phase qualify for cyclic_lut/cyclic_time that it couldn't before.
  # This is the precise, load-bearing claim for this feature.
  #
  # Uses controlled synthetic data with a GUARANTEED genuinely-open trailing
  # cycle, rather than the package's own cycledata example. After the 0.1.7
  # max_window fix (see the BUG FIX test above), cycledata itself no longer
  # reliably demonstrates this: every confirmed ovulation in that example
  # dataset turns out to already have a real closing menses somewhere later
  # in the record (just not within the old buggy 20-day window) -- so under
  # correct semantics, impute_next_menses now fires ZERO times on cycledata.
  # That is not a regression; it is confirmation that every prior "gain" this
  # test saw on cycledata was the bug itself, not the feature working.
  mk <- function(id, ov_day, mens_days, n) data.frame(
    id = id, daterated = as.Date("2026-01-01") + 0:(n - 1),
    menses = as.integer((0:(n - 1)) %in% (mens_days - 1)),
    ovtoday = as.integer((0:(n - 1)) == (ov_day - 1)))
  synth <- mk("A", ov_day = 15, mens_days = c(1), n = 40)  # no closing menses anywhere

  off <- pacts_scaling(synth, id = id, date = daterated, menses = menses, ovtoday = ovtoday)
  on  <- pacts_scaling(synth, id = id, date = daterated, menses = menses, ovtoday = ovtoday,
                       impute_next_menses = TRUE)
  expect_gt(sum(!is.na(on$cyclic_time)), sum(!is.na(off$cyclic_time)))
  expect_true("menses_impute" %in% names(on))
  expect_gt(sum(on$menses_impute, na.rm = TRUE), 0)
})

test_that("impute_next_menses = TRUE now correctly fires zero times on cycledata (no genuinely open cycles)", {
  on  <- pacts_scaling(cycledata, id = id, date = daterated, menses = menses, ovtoday = ovtoday,
                       impute_next_menses = TRUE)
  expect_true("menses_impute" %in% names(on))
  expect_equal(sum(on$menses_impute, na.rm = TRUE), 0)
})
