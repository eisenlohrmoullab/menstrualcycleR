# RELEASE-BLOCKER FIX (pre-release adversarial review, 2026-08-20, finding R2): calculate_mcyclength()
# and impute_next_menses_onsets() coerced non-Date date columns via lubridate::ymd(), which
# returns NA for any POSIXct value with a nonzero time-of-day. calculate_mcyclength() then
# silently dropped those NA rows and tidyr::complete() refilled them as empty calendar days --
# so a single non-midnight timestamp (e.g. a 09:00 survey entry) on an ovulation day silently
# deleted that ovulation with no warning, and uniformly-timestamped input crashed opaquely inside
# complete() ("'from' must be a finite number"). Fixed by coercing POSIXct via as.Date() in its
# own timezone (dropping the time-of-day directly) instead of re-parsing its string
# representation through ymd().

mk_posix <- function(n, offset_row = NA_integer_, offset_secs = 3600, tz = "UTC") {
  dates <- as.POSIXct(as.Date("2026-01-01") + 0:(n - 1), tz = tz)
  if (!is.na(offset_row)) dates[offset_row] <- dates[offset_row] + offset_secs
  dates
}

test_that("BUG FIX (R2): a single non-midnight timestamp on the ovulation day does not silently delete it", {
  n <- 20
  df <- data.frame(id = "A", date = mk_posix(n, offset_row = 15),
                    menses = as.integer((0:(n - 1)) == 0), ovtoday = as.integer((0:(n - 1)) == 14))
  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)
  expect_equal(nrow(out), n)
  expect_equal(sum(out$ovtoday, na.rm = TRUE), 1)
  expect_true(any(!is.na(out$cyclic_time)))
})

test_that("BUG FIX (R2): uniformly non-midnight timestamps no longer crash inside tidyr::complete()", {
  n <- 20
  dates <- mk_posix(n) + 9 * 3600  # every timestamp at 09:00
  df <- data.frame(id = "A", date = dates, menses = as.integer((0:(n - 1)) == 0),
                    ovtoday = as.integer((0:(n - 1)) == 14))
  expect_no_error(out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday))
  expect_equal(nrow(out), n)
  expect_equal(sum(out$ovtoday, na.rm = TRUE), 1)
})

test_that("midnight-UTC POSIXct input (the previously-working case) is unaffected", {
  n <- 20
  df <- data.frame(id = "A", date = mk_posix(n), menses = as.integer((0:(n - 1)) == 0),
                    ovtoday = as.integer((0:(n - 1)) == 14))
  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)
  expect_equal(sum(out$ovtoday, na.rm = TRUE), 1)
})

test_that("a genuinely unparseable date still warns and drops that row (coercion failure is not silently swallowed)", {
  df <- data.frame(id = "A", date = c("2026-01-01", "not-a-date", "2026-01-03"),
                    menses = c(1L, 0L, 0L), ovtoday = c(0L, 0L, 1L))
  expect_warning(
    out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday),
    "new NA"
  )
})

test_that("ordinary character-coded date columns are completely unaffected by the POSIXct fix", {
  # nrow legitimately grows beyond nrow(cycledata) via tidyr::complete()'s
  # calendar-filling within each id's date range -- that's pre-existing,
  # unrelated behavior. What must be unaffected is that no rows/ovulations
  # are lost by the coercion path itself.
  out <- pacts_scaling(cycledata, id = id, date = daterated, menses = menses, ovtoday = ovtoday)
  expect_equal(sum(out$ovtoday, na.rm = TRUE), sum(cycledata$ovtoday, na.rm = TRUE))
  expect_equal(sum(out$menses, na.rm = TRUE), sum(cycledata$menses, na.rm = TRUE))
})

test_that("impute_next_menses_onsets() correctly handles a non-midnight POSIXct date column", {
  n <- 40
  df <- data.frame(id = "A", date = mk_posix(n, offset_row = 15),
                    menses = as.integer((0:(n - 1)) == 0), ovtoday = as.integer((0:(n - 1)) == 14))
  imp <- impute_next_menses_onsets(df, id, date, menses, ovtoday)
  expect_equal(sum(imp$ovtoday, na.rm = TRUE), 1)
  expect_equal(sum(imp$menses_impute), 1)
})
