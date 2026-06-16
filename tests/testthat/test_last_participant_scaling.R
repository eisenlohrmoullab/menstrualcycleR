# Regression test for the "last participant" scaling bug.
#
# Cause: the lutmax / folmax / folmax_impute run-length loops in helper.R
# detected the end of a phase by looking one row ahead and iterated only to
# `nrow(data) - 1`. The final row of the dataset was therefore never treated as
# the end of a run, so whichever participant occupied the last row lost the
# scaling for their final phase (e.g. scaled_cycleday all-NA across the luteal
# phase). Reordering participants moved the bug to the new last participant.
#
# Invariant under test: a participant's scaled output must be identical whether
# or not they are the last participant in the dataset.

# One clean, fully scalable cycle: menses onset day 1, ovulation day 15,
# luteal days 16-28, closing menses day 29.
make_cycle <- function(id, start = "2024-01-01") {
  data.frame(
    id        = id,
    daterated = as.Date(start) + 0:28,
    menses    = c(1, rep(0, 27), 1),
    ovtoday   = c(rep(0, 14), 1, rep(0, 14)),
    symptom   = 1
  )
}

scaled_cols <- c(
  "scaled_cycleday", "scaled_cycleday_ov",
  "scaled_cycleday_impute", "scaled_cycleday_imp_ov"
)

scale_quiet <- function(df) {
  suppressMessages(
    pacts_scaling(df, id = id, date = daterated, menses = menses, ovtoday = ovtoday)
  ) |>
    dplyr::arrange(id, daterated)
}

test_that("scaling of a participant does not depend on their position in the dataset", {
  target <- make_cycle(79, "2024-01-01")
  trailer <- make_cycle(80, "2024-03-01")

  as_last  <- scale_quiet(target)
  as_inner <- scale_quiet(dplyr::bind_rows(target, trailer))

  t_last  <- dplyr::filter(as_last,  id == 79)
  t_inner <- dplyr::filter(as_inner, id == 79)

  # Position invariance: identical scaled output in both arrangements.
  for (col in scaled_cols) {
    expect_equal(t_last[[col]], t_inner[[col]], info = col)
  }
})

test_that("the last participant's luteal phase is scaled (not all NA)", {
  res <- scale_quiet(make_cycle(79, "2024-01-01"))
  luteal <- res |>
    dplyr::filter(id == 79) |>
    dplyr::mutate(day = as.integer(daterated - min(daterated)) + 1L) |>
    dplyr::filter(day > 15 & menses != 1)        # strictly post-ovulation, pre next-menses

  expect_false(all(is.na(luteal$scaled_cycleday)))
  # First post-ovulation day anchors the luteal phase at -1.
  expect_equal(min(luteal$scaled_cycleday, na.rm = TRUE), -1)
})
