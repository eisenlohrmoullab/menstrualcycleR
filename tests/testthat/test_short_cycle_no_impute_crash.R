# A complete cycle of 14 days or fewer used to CRASH ovulation imputation.
#
# Imputed ovulation is placed 14 days before the next menses, so the implied
# follicular length is `mcyclength - 14`. For a cycle of <= 14 days that is zero
# or negative, and it reached `seq_len()` in calculate_ovtoday_impute(), which
# errors with "argument must be coercible to non-negative integer" (or
# "replacement has length zero" at exactly 14).
#
# Reachable whenever lower_cyclength_bound < 15. The bundled Shiny app offers a
# minimum of 10 (inst/shiny/ui.R), so it was reachable from the GUI.
#
# Such cycles now simply get no imputed ovulation.

test_that("a complete cycle shorter than 15 days does not crash imputation", {
  short <- data.frame(
    id        = "p1",
    daterated = as.Date("2024-01-01") + 0:37,
    # 12-day cycle, then a 26-day cycle
    menses    = c(1, rep(0, 11), 1, rep(0, 25)),
    ovtoday   = 0,
    symptom   = 0
  )
  expect_no_error(
    pacts_scaling(short, id = id, date = daterated, menses = menses,
                  ovtoday = ovtoday,
                  lower_cyclength_bound = 10, upper_cyclength_bound = 35)
  )
})

test_that("a 14-day cycle gets no imputed ovulation rather than erroring", {
  exactly14 <- data.frame(
    id        = "p1",
    daterated = as.Date("2024-01-01") + 0:39,
    menses    = c(1, rep(0, 13), 1, rep(0, 25)),
    ovtoday   = 0,
    symptom   = 0
  )
  out <- pacts_scaling(exactly14, id = id, date = daterated, menses = menses,
                       ovtoday = ovtoday,
                       lower_cyclength_bound = 10, upper_cyclength_bound = 35)
  first_cycle <- out[out$daterated < as.Date("2024-01-15"), ]
  expect_true(all(is.na(first_cycle$ovtoday_impute) | first_cycle$ovtoday_impute == 0))
})

test_that("the default bounds are unaffected by the guard", {
  # 26-day cycle, well inside [21, 35] -- must still impute as before
  normal <- data.frame(
    id        = "p1",
    daterated = as.Date("2024-01-01") + 0:51,
    menses    = c(1, rep(0, 25), 1, rep(0, 25)),
    ovtoday   = 0,
    symptom   = 0
  )
  out <- pacts_scaling(normal, id = id, date = daterated, menses = menses,
                       ovtoday = ovtoday)
  expect_true(any(out$ovtoday_impute == 1, na.rm = TRUE))
})
