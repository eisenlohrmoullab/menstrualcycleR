# BUG FIX (1.1.0): the follicular-phase run was closed on a participant's FIRST row whenever the
# previous row in the data frame -- the preceding participant's last row -- was an ovulation day.
# The check used row i-1 without confirming it belonged to the same id, so the whole first
# follicular phase of that participant stayed unscaled. Found on real data (3 of 125 participants).

test_that("a participant whose predecessor ends on an ovulation day still gets their first follicular phase scaled", {
  # A: menses day 1, ovulation on the LAST observed day (day 14). B: menses day 1, ov day 14, menses day 29.
  a <- data.frame(id = "A", date = as.Date("2026-01-01") + 0:13, menses = as.integer((0:13) == 0), ovtoday = as.integer((0:13) == 13))
  b <- data.frame(id = "B", date = as.Date("2026-03-01") + 0:34, menses = as.integer((0:34) %in% c(0, 28)), ovtoday = as.integer((0:34) == 13))
  both  <- pacts_scaling(rbind(a, b), id = id, date = date, menses = menses, ovtoday = ovtoday)
  alone <- pacts_scaling(b,           id = id, date = date, menses = menses, ovtoday = ovtoday)
  fb <- both[both$id == "B", ]; fb <- fb[order(fb$date), ]
  expect_equal(fb$cyclic_time_impute, alone$cyclic_time_impute[order(alone$date)])
  expect_equal(fb$cyclic_time,        alone$cyclic_time[order(alone$date)])
  expect_true(all(is.finite(fb$cyclic_time_impute[fb$date < as.Date("2026-03-15")])))   # follicular days 1-14 scaled
})
