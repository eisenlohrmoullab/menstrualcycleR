# Tests for the opt-in leading-ovulation imputation (impute_leading_ovulation).
# Guarantee: the default (FALSE) changes NOTHING; the opt-in scales the pre-first-menses days.

test_that("impute_leading_ovulation defaults are inert (published behavior unchanged)", {
  a <- pacts_scaling(cycledata, id = id, date = daterated, menses = menses, ovtoday = ovtoday)
  b <- pacts_scaling(cycledata, id = id, date = daterated, menses = menses, ovtoday = ovtoday,
                     impute_leading_ovulation = FALSE)
  expect_equal(a, b)
  expect_false("ovtoday_leading_impute" %in% names(a))
})

test_that("leading days before the first menses are scaled as a luteal tail when opted in", {
  # 10 observed days, then menses on day 11, confirmed ov day 25, menses day 39.
  n <- 45
  df <- data.frame(id = "A", date = as.Date("2026-01-01") + 0:(n - 1),
                   menses  = as.integer((0:(n - 1)) %in% c(10, 38)),
                   ovtoday = as.integer((0:(n - 1)) == 24))
  off <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)
  on  <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                       impute_leading_ovulation = TRUE)
  lead <- on$date < as.Date("2026-01-11")
  expect_true(all(is.na(off$cyclic_time_impute[off$date < as.Date("2026-01-11")])))   # before: unscaled
  expect_true(all(is.finite(on$cyclic_time_impute[lead])))                             # after: scaled
  after_anchor <- lead & on$date > as.Date("2025-12-27")
  expect_true(all(on$cyclic_time_impute[after_anchor] < 0))                             # as luteal (negative side); the anchor day itself is +1
  expect_equal(sum(on$ovtoday_leading_impute == 1), 1)
  expect_equal(as.character(on$date[on$ovtoday_leading_impute == 1]), "2025-12-27")     # 15 days before Jan 11: a blank added row
  expect_true(all(is.na(on$cyclic_time[lead])))                                         # confirmed-only columns untouched
  # the observed cycle is identical under both settings
  same <- on$date >= as.Date("2026-01-11")
  expect_equal(on$cyclic_time_impute[same], off$cyclic_time_impute[off$date >= as.Date("2026-01-11")])
})

test_that("no imputation when a confirmed ovulation already precedes the first menses, or when nothing precedes it", {
  n <- 40
  df1 <- data.frame(id = "B", date = as.Date("2026-01-01") + 0:(n - 1),
                    menses = as.integer((0:(n - 1)) == 12), ovtoday = as.integer((0:(n - 1)) == 2))
  df2 <- data.frame(id = "C", date = as.Date("2026-01-01") + 0:(n - 1),
                    menses = as.integer((0:(n - 1)) %in% c(0, 28)), ovtoday = as.integer((0:(n - 1)) == 13))
  o1 <- pacts_scaling(df1, id = id, date = date, menses = menses, ovtoday = ovtoday, impute_leading_ovulation = TRUE)
  o2 <- pacts_scaling(df2, id = id, date = date, menses = menses, ovtoday = ovtoday, impute_leading_ovulation = TRUE)
  expect_equal(sum(o1$ovtoday_leading_impute), 0)
  expect_equal(sum(o2$ovtoday_leading_impute), 0)
  expect_equal(nrow(o2), n)
})
