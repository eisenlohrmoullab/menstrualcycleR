# Extracted from test_impute_leading_ovulation.R:24

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "menstrualcycleR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
n <- 45
df <- data.frame(id = "A", date = as.Date("2026-01-01") + 0:(n - 1),
                   menses  = as.integer((0:(n - 1)) %in% c(10, 38)),
                   ovtoday = as.integer((0:(n - 1)) == 24))
off <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)
on  <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                       impute_leading_ovulation = TRUE)
lead <- on$date < as.Date("2026-01-11")
expect_true(all(is.na(off$cyclic_time_impute[off$date < as.Date("2026-01-11")])))
expect_true(all(is.finite(on$cyclic_time_impute[lead])))
expect_true(all(on$cyclic_time_impute[lead] < 0))
