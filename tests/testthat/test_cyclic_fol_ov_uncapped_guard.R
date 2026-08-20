# BUG FIX (pre-release adversarial review, 2026-08-20, finding S3): cyclic_fol_ov_uncapped's
# double-count guard reused percfol_ov (built from the CAPPED folperc), which is NA both for a
# genuine luteal/follicular boundary overlap AND simply because the follicular phase exceeds
# follicular_phase_max_days -- the exact case the fallback exists to cover. That guard was itself
# added earlier the same day as a defense-in-depth hardening (mirroring cyclic_fol_ov's own
# guard) with no demonstrated trigger at the time; the adversarial review found the real,
# demonstrated cost: it silently zeroed ALL ov-centered fallback coverage for over-cap follicular
# phases, contradicting the documented "identical fallback" claim for cyclic_time_impute vs
# cyclic_time_imp_ov. Fixed by building a separate, uncapped-appropriate dedup signal
# (percfol_uncapped_dedup, from folperc_uncapped) instead of reusing the capped percfol_ov.

test_that("BUG FIX (S3): cyclic_time_imp_ov gets fallback coverage for an over-cap follicular phase, matching cyclic_time_impute", {
  # menses day 1, ov day 28 (27d follicular, over the 25d default cap),
  # closing menses day 40 (12d luteal, in-band) -- 39-day cycle, bounds [20,45].
  n <- 46
  df <- data.frame(id = "A", date = as.Date("2026-01-01") + 0:(n - 1),
                    menses = as.integer((0:(n - 1)) %in% c(0, 39)),
                    ovtoday = as.integer((0:(n - 1)) == 27))
  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday,
                        lower_cyclength_bound = 20, upper_cyclength_bound = 45)

  foll_rows <- out$date >= as.Date("2026-01-01") & out$date < as.Date("2026-01-28")
  expect_true(all(is.na(out$cyclic_time[foll_rows])))       # strict path correctly excludes it
  expect_true(all(!is.na(out$cyclic_time_impute[foll_rows])))    # menses-anchored fallback covers it
  expect_true(all(!is.na(out$cyclic_time_imp_ov[foll_rows])))    # ov-anchored fallback must too
  expect_true(all(out$cyclic_time_imp_ov_extended_phase[foll_rows] == 1))
})

test_that("the luteal/follicular boundary-day dedup still protects cyclic_time_imp_ov (no leaked double-count)", {
  # Two consecutive complete, in-cap cycles -- every day in both cycles must
  # have exactly one source (no unexpected NA gaps at the luteal/follicular seam).
  df <- data.frame(id = "B", date = as.Date("2026-01-01") + 0:59,
                    menses = as.integer((0:59) %in% c(0, 28, 56)),
                    ovtoday = as.integer((0:59) %in% c(13, 41)))
  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)
  in_scope <- out$date < as.Date("2026-02-26")
  expect_equal(sum(is.na(out$cyclic_time_imp_ov[in_scope])), 0)
})

test_that("an ordinary in-cap cycle's cyclic_time_imp_ov is completely unaffected by the S3 fix", {
  df <- data.frame(id = "C", date = as.Date("2026-01-01") + 0:34,
                    menses = as.integer((0:34) %in% c(0, 30)), ovtoday = as.integer((0:34) == 14))
  out <- pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)
  in_cycle <- out$date <= as.Date("2026-01-31")  # excludes the trailing right-censored fragment
  expect_true(all(!is.na(out$cyclic_time_imp_ov[in_cycle])))
  expect_true(all(out$cyclic_time_imp_ov_extended_phase[in_cycle] == 0))
})
