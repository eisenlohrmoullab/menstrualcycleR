# Tests for pacts_scaling()'s input validation, added because the function
# previously had none: a missing column threw an opaque rlang stack trace, a
# character-coded menses/ovtoday ("yes"/"no") produced NO error at all and
# silently returned every derived column as NA, and passing a quoted column
# name (a natural mistake coming from base R) failed deep inside tidyr with
# an unrelated message. All four should now fail loudly and specifically.

mk <- function(n = 20) data.frame(
  id      = "A",
  date    = as.Date("2026-01-01") + 0:(n - 1),
  menses  = as.integer((0:(n - 1)) %in% c(0)),
  ovtoday = as.integer((0:(n - 1)) %in% c(13))
)

test_that("a missing column names the argument and lists available columns", {
  df <- mk()
  df$ovtoday <- NULL
  expect_error(
    pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday),
    "ovtoday.*not found in .data.|Available columns"
  )
})

test_that("a quoted column name is caught with a specific message, not a downstream failure", {
  df <- mk()
  expect_error(
    pacts_scaling(df, id = "id", date = date, menses = menses, ovtoday = ovtoday),
    "bare, unquoted column name"
  )
})

test_that("character-coded menses is rejected loudly instead of silently returning all-NA columns", {
  df <- mk()
  df$menses <- ifelse(df$menses == 1, "yes", "no")
  expect_error(
    pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday),
    "must be numeric 0/1"
  )
})

test_that("character-coded ovtoday is rejected loudly", {
  df <- mk()
  df$ovtoday <- ifelse(df$ovtoday == 1, "yes", "no")
  expect_error(
    pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday),
    "must be numeric 0/1"
  )
})

test_that("a non-0/1 numeric value in menses or ovtoday is rejected with the offending value named", {
  df <- mk()
  df$menses[3] <- 2
  expect_error(
    pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday),
    "must contain only 0, 1, or NA"
  )
})

test_that("NA is still allowed in menses/ovtoday (not treated as an invalid value)", {
  df <- mk()
  df$ovtoday[5] <- NA
  expect_no_error(
    pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)
  )
})

test_that("logical (TRUE/FALSE) menses/ovtoday still passes validation", {
  df <- mk()
  df$menses <- as.logical(df$menses)
  df$ovtoday <- as.logical(df$ovtoday)
  expect_no_error(
    pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)
  )
})

test_that("ordinary valid input is completely unaffected by the new validation", {
  a <- pacts_scaling(cycledata, id = id, date = daterated, menses = menses, ovtoday = ovtoday)
  expect_true(nrow(a) > 0)
  expect_true("cyclic_time" %in% names(a))
})
