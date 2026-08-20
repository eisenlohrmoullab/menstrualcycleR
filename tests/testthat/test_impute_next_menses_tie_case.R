# BUG FIX (pre-release adversarial review, 2026-08-20, finding S2): the closing-menses search in
# impute_next_menses_onsets() required the closing menses to fall STRICTLY before the id's next
# confirmed ovulation (m_date < next_ov_date). When the only closing menses for an ovulation
# lands exactly ON the next ovulation's own date (a tie), the strict `<` excluded it, treating the
# earlier ovulation as unclosed and fabricating a phantom onset despite a real closing menses
# existing -- the exact bug class the cross-ovulation-scoping fix (same day, earlier) exists to
# prevent. Fixed by using `<=` instead.

test_that("BUG FIX (S2): a closing menses landing exactly on the next ovulation's date correctly closes the EARLIER cycle (no phantom for it)", {
  # ov1 day 10 (no closing menses of its own before ov2); the only later
  # menses onset falls on day 29, which coincides exactly with ov2's own
  # date. That real menses must close ov1's cycle. ov2 itself has no
  # closing menses anywhere in the truncated record, so it correctly still
  # gets its own +14 phantom -- the fix targets ov1 specifically, not ov2.
  n <- 30
  df <- data.frame(id = "A", date = as.Date("2026-01-01") + 0:(n - 1),
                    menses = as.integer((0:(n - 1)) == 0),
                    ovtoday = as.integer((0:(n - 1)) %in% c(9, 28)))
  df$menses[df$date == as.Date("2026-01-01") + 28] <- 1L  # real menses, same day as ov2

  imp <- impute_next_menses_onsets(df, id, date, menses, ovtoday)

  ov1_phantom_date <- as.Date("2026-01-01") + 9 + 14  # 2026-01-24
  expect_equal(imp$menses_impute[imp$date == ov1_phantom_date], 0)
  # ov2's own legitimate phantom (no closing menses after it) is untouched.
  ov2_phantom_date <- as.Date("2026-01-01") + 28 + 14  # 2026-02-12
  expect_equal(sum(imp$menses_impute), 1)
  expect_equal(imp$date[imp$menses_impute == 1], ov2_phantom_date)
})

test_that("a closing menses strictly before the next ovulation still works as before (not a regression from < to <=)", {
  n <- 40
  df <- data.frame(id = "A", date = as.Date("2026-01-01") + 0:(n - 1),
                    menses = as.integer((0:(n - 1)) %in% c(0, 24)),
                    ovtoday = as.integer((0:(n - 1)) %in% c(9, 38)))
  imp <- impute_next_menses_onsets(df, id, date, menses, ovtoday)
  # ov1 (day10) is closed by the real menses on day25, well before ov2 (day39)
  expect_equal(sum(imp$menses_impute[imp$date < as.Date("2026-01-01") + 30]), 0)
})
