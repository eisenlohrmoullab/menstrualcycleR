## Proposed feature (branch wip/impute-ovulation-flag): pacts_scaling(impute_ovulation=)
## Lets an upstream anchor-prep step place/curate imputed ovulations and have pacts_scaling
## RESPECT them instead of mechanically re-imputing. Default TRUE preserves original behavior.

make_df <- function() {
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 90)
  df <- data.frame(id = 1L, daterated = dates, menses = 0L, ovtoday = 0L)
  df$menses[df$daterated %in% as.Date(c("2020-01-01","2020-01-29","2020-02-26","2020-03-25"))] <- 1L
  df$ovtoday[df$daterated == as.Date("2020-01-14")] <- 1L  # confirmed ov, cycle 1 only
  df
}

test_that("impute_ovulation default TRUE reproduces the original behavior", {
  df <- make_df()
  r_default  <- pacts_scaling(df, id, daterated, menses, ovtoday, 20, 43)
  r_explicit <- pacts_scaling(df, id, daterated, menses, ovtoday, 20, 43, impute_ovulation = TRUE)
  expect_identical(r_default$ovtoday_impute, r_explicit$ovtoday_impute)
  expect_gt(sum(r_default$ovtoday_impute == 1, na.rm = TRUE), 0)  # unconfirmed cycles do get imputed
})

test_that("impute_ovulation = FALSE respects a pre-placed ovtoday_impute and adds none of its own", {
  df <- make_df()
  df$ovtoday_impute <- 0L
  df$ovtoday_impute[df$daterated == as.Date("2020-02-12")] <- 1L
  r <- pacts_scaling(df, id, daterated, menses, ovtoday, 20, 43, impute_ovulation = FALSE)
  expect_equal(r$ovtoday_impute[r$date == as.Date("2020-02-12")], 1L)   # my anchor kept
  expect_equal(sum(r$ovtoday_impute == 1, na.rm = TRUE), 1L)             # nothing mechanically added
  expect_true(any(!is.na(r$cyclic_time_impute)))                        # scaling still runs off my anchor
})

test_that("impute_ovulation = FALSE with no ovtoday_impute column yields zero imputed ovulations", {
  df <- make_df()
  r <- pacts_scaling(df, id, daterated, menses, ovtoday, 20, 43, impute_ovulation = FALSE)
  expect_equal(sum(r$ovtoday_impute == 1, na.rm = TRUE), 0L)
})
