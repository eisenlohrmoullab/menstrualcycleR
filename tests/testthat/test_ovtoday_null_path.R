# `ovtoday` has three states that R can tell apart, and they must stay apart:
#
#   omitted       -> error. Someone who forgot the argument but HAS ovulation data must
#                    not silently receive an all-imputed analysis.
#   NULL          -> affirmative "no ovulation biomarker collected". Column created,
#                    message emitted, every cycle takes the imputed path.
#   a column name -> validated; a typo errors and names the available columns.
#
# The hazard being guarded against is collapsing "omitted" into "NULL". That is not
# hypothetical: the first implementation of this feature did exactly that, because giving
# the argument a default of NULL makes rlang::quo_is_missing() permanently FALSE. Only
# base::missing(), read before enquo() rebinds the name, separates the two.

test_that("omitting ovtoday is an error, not a silent fallback", {
  d <- cycledata
  d$ovtoday <- NULL
  expect_error(
    pacts_scaling(d, id = id, date = daterated, menses = menses),
    "ovtoday"
  )
})

test_that("the omission error names both ways forward and leaks no internal argument", {
  d <- cycledata
  d$ovtoday <- NULL
  msg <- tryCatch(
    pacts_scaling(d, id = id, date = daterated, menses = menses),
    error = conditionMessage
  )
  expect_match(msg, "NULL")
  expect_false(grepl('argument "x" is missing', msg, fixed = TRUE))
})

test_that("omitting ovtoday errors even when the column is present", {
  # The dangerous case: the caller has ovulation data and simply forgot the argument.
  expect_error(
    pacts_scaling(cycledata, id = id, date = daterated, menses = menses),
    "ovtoday"
  )
})

test_that("ovtoday = NULL scales every cycle through the imputed path, and says so", {
  d <- cycledata
  d$ovtoday <- NULL
  expect_message(
    res <- pacts_scaling(d, id = id, date = daterated, menses = menses, ovtoday = NULL),
    "imputed for every cycle"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(sum(!is.na(res$cyclic_time)), 0)
  expect_gt(sum(!is.na(res$cyclic_time_impute)), 0)
})

test_that("ovtoday = NULL matches supplying an all-NA column by hand", {
  d <- cycledata
  d$ovtoday <- NULL
  via_null <- suppressMessages(
    pacts_scaling(d, id = id, date = daterated, menses = menses, ovtoday = NULL)
  )
  d_na <- cycledata
  d_na$ovtoday <- NA_real_
  via_column <- pacts_scaling(d_na, id = id, date = daterated,
                              menses = menses, ovtoday = ovtoday)
  expect_identical(via_null[names(via_column)], via_column)
})

test_that("a mistyped column name still errors instead of being auto-created", {
  # This is what auto-creating on absence would have destroyed.
  msg <- tryCatch(
    pacts_scaling(cycledata, id = id, date = daterated,
                  menses = menses, ovtoday = ovtody),
    error = conditionMessage
  )
  expect_match(msg, "not found in")
  expect_match(msg, "Available columns")
})

test_that("NULL over an existing ovtoday column warns but is honored", {
  expect_warning(
    res <- suppressMessages(
      pacts_scaling(cycledata, id = id, date = daterated,
                    menses = menses, ovtoday = NULL)
    ),
    "already has an"
  )
  expect_equal(sum(!is.na(res$cyclic_time)), 0)
})

test_that("an ovtoday binding in the caller's scope cannot leak into the NULL path", {
  # The synthesised quosure carries an empty environment precisely so that the data
  # mask, not the calling frame, resolves the symbol.
  d <- cycledata
  d$ovtoday <- NULL
  ovtoday <- "SABOTAGE"
  res <- suppressMessages(
    pacts_scaling(d, id = id, date = daterated, menses = menses, ovtoday = NULL)
  )
  expect_equal(sum(!is.na(res$cyclic_time_impute)), 744)
})

test_that("the NULL path composes with impute_next_menses and summary_ovulation", {
  d <- cycledata
  d$ovtoday <- NULL
  res <- suppressMessages(
    pacts_scaling(d, id = id, date = daterated, menses = menses,
                  ovtoday = NULL, impute_next_menses = TRUE)
  )
  expect_s3_class(res, "data.frame")

  plain <- suppressMessages(
    pacts_scaling(d, id = id, date = daterated, menses = menses, ovtoday = NULL)
  )
  expect_type(summary_ovulation(plain), "list")
})

test_that("supplying a real ovtoday column is unaffected", {
  res <- pacts_scaling(cycledata, id = id, date = daterated,
                       menses = menses, ovtoday = ovtoday)
  expect_equal(sum(!is.na(res$cyclic_time)), 358)
})
