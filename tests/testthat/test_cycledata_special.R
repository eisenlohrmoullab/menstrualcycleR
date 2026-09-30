# Each person in cycledata_special exists to demonstrate one setting, and the
# `case` column states what that setting does to them. These checks hold the
# labels to it: if a default changes, or a code path stops firing, the person
# stops demonstrating their case and this fails -- instead of the dataset
# quietly shipping a documented claim that is no longer true.

sc <- function(...) {
  suppressMessages(suppressWarnings(pacts_scaling(
    cycledata_special, id = id, date = daterated,
    menses = menses, ovtoday = ovtoday, ...)))
}
cov_ct  <- function(x, k) sum(!is.na(x$cyclic_time[x$id == k]))
cov_cti <- function(x, k) sum(!is.na(x$cyclic_time_impute[x$id == k]))
nrow_id <- function(x, k) sum(x$id == k)

test_that("the dataset itself is shaped as documented", {
  expect_equal(nrow(cycledata_special), 998L)
  expect_named(cycledata_special,
               c("id", "daterated", "symptom", "menses", "ovtoday", "case"))
  expect_equal(sort(unique(cycledata_special$id)), 1:15)
  # one case label per person, and no person sharing another's label
  labs <- unique(cycledata_special[, c("id", "case")])
  expect_equal(nrow(labs), 15L)
  expect_equal(length(unique(labs$case)), 15L)
  expect_true(all(cycledata_special$menses %in% c(0L, 1L)))
  expect_true(all(cycledata_special$ovtoday %in% c(0L, 1L)))
  # no duplicate person-days, which pacts_scaling() requires of its input
  expect_false(any(duplicated(cycledata_special[, c("id", "daterated")])))
})

test_that("person 1 is ordinary: everything but the open trailing cycle scales", {
  b <- sc()
  expect_equal(cov_ct(b, 1), 85L)
  expect_equal(cov_ct(b, 1), cov_cti(b, 1))   # nothing to impute
})

test_that("leading ovulation gains 15 days for persons 2 and 3, and nobody else", {
  b <- sc(); l <- sc(impute_leading_ovulation = TRUE)
  gained <- vapply(1:15, function(k) cov_cti(l, k) - cov_cti(b, k), integer(1))
  expect_equal(which(gained != 0), c(2L, 3L, 15L))
  expect_equal(gained[c(2, 3, 15)], c(15L, 15L, 15L)) # the default placement, 15 days back
  # person 2's imputed ovulation precedes their first row, so rows are fabricated
  expect_equal(nrow_id(l, 2) - nrow_id(b, 2), 7L)
  expect_equal(nrow_id(l, 3) - nrow_id(b, 3), 0L)     # person 3's was already rated
  # and the anchor is flagged for exactly those two people
  flagged <- tapply(l$ovtoday_leading_impute == 1, l$id, sum)
  expect_equal(as.integer(names(flagged)[flagged > 0]), c(2L, 3L, 15L))
})

test_that("person 4's confirmed leading ovulation scales with no setting, and blocks the imputed one", {
  b <- sc(); l <- sc(impute_leading_ovulation = TRUE)
  expect_equal(cov_ct(b, 4), 43L)                     # already covered, confirmed column
  expect_equal(cov_cti(l, 4), cov_cti(b, 4))          # the setting declines
  expect_equal(sum(l$ovtoday_leading_impute[l$id == 4] == 1, na.rm = TRUE), 0L)
})

test_that("impute_next_menses closes persons 5 and 14, in the confirmed columns too", {
  b <- sc(); n <- sc(impute_next_menses = TRUE)
  gained <- vapply(1:15, function(k) cov_cti(n, k) - cov_cti(b, k), integer(1))
  expect_equal(which(gained != 0), c(5L, 14L, 15L))
  expect_equal(gained[c(5, 14, 15)], c(14L, 14L, 14L))
  # not just the imputed columns -- the confirmed ones gain the same days
  expect_equal(cov_ct(n, 5) - cov_ct(b, 5), 14L)
  expect_equal(cov_ct(n, 14) - cov_ct(b, 14), 14L)
  expect_equal(nrow_id(n, 5) - nrow_id(b, 5), 5L)     # fabricated to reach the onset
})

test_that("the cycle-length bounds leave persons 6 and 7 empty, and widening them does not", {
  b <- sc()
  # the out-of-range cycle gets no imputed ovulation at the defaults
  expect_equal(sum(b$ovtoday_impute[b$id %in% c(6, 7)] == 1, na.rm = TRUE), 0L)
  w <- sc(lower_cyclength_bound = 18, upper_cyclength_bound = 43)
  expect_equal(sum(w$ovtoday_impute[w$id %in% c(6, 7)] == 1, na.rm = TRUE), 2L)
  expect_gt(sum(!is.na(w$cyclic_time_impute[w$id %in% c(6, 7)])),
            sum(!is.na(b$cyclic_time_impute[b$id %in% c(6, 7)])))
})

test_that("persons 8 and 9 are recovered by the phase-cap fallback, and flagged", {
  b <- sc()
  expect_equal(cov_ct(b, 8), 14L);  expect_equal(cov_cti(b, 8), 36L)
  expect_equal(cov_ct(b, 9), 10L);  expect_equal(cov_cti(b, 9), 36L)
  flag <- function(k) sum(b$cyclic_time_impute_extended_phase[b$id == k] == 1, na.rm = TRUE)
  expect_equal(flag(8), 21L); expect_equal(flag(9), 26L)
  # widening the cap each one crosses returns those days to cyclic_time itself
  expect_equal(cov_ct(sc(luteal_phase_max_days = 25), 8) - cov_ct(b, 8), 22L)
  expect_equal(cov_ct(sc(follicular_phase_max_days = 30), 9) - cov_ct(b, 9), 26L)
})

test_that("person 10's short luteal phase is recovered by nothing but the floor itself", {
  b <- sc()
  expect_equal(cov_ct(b, 10), 22L)
  expect_equal(cov_cti(b, 10), 22L)                   # no fallback: floors still apply
  # the ceilings are irrelevant to a floor violation
  expect_equal(cov_ct(sc(luteal_phase_max_days = 25, follicular_phase_max_days = 30), 10), 22L)
  expect_equal(cov_ct(sc(luteal_phase_min_days = 4), 10), 27L)
})

test_that("person 11's diary hole comes back as rows with no rating", {
  b <- sc()
  expect_equal(nrow_id(b, 11) - sum(cycledata_special$id == 11), 11L)
  # `case` reads NA on exactly the rows the package added
  expect_equal(sum(is.na(b$case[b$id == 11])), 11L)
  expect_equal(sum(is.na(b$symptom[b$id == 11]) & is.na(b$case[b$id == 11])), 11L)
})

test_that("person 12 is imputed-only and person 13 is unscalable", {
  b <- sc()
  expect_equal(cov_ct(b, 12), 0L)                     # nothing confirmed
  expect_equal(cov_cti(b, 12), 86L)
  expect_equal(sum(b$ovtoday_impute[b$id == 12] == 1, na.rm = TRUE), 3L)
  # person 13 has no anchor at all: no setting reaches them
  for (x in list(b, sc(impute_leading_ovulation = TRUE), sc(impute_next_menses = TRUE),
                 sc(lower_cyclength_bound = 18, upper_cyclength_bound = 43))) {
    expect_equal(sum(!is.na(x$cyclic_time[x$id == 13])), 0L)
    expect_equal(sum(!is.na(x$cyclic_time_impute[x$id == 13])), 0L)
  }
})

test_that("person 14's lone confirmed ovulation scales even with no cycle around it", {
  b <- sc()
  expect_equal(cov_ct(b, 14), 1L)
  expect_equal(b$cyclic_time[b$id == 14 & b$ovtoday == 1], 1)
  expect_true(all(is.na(b$mcyclength[b$id == 14])))
})

test_that("the two opt-in settings compose without interfering", {
  b <- sc(); l <- sc(impute_leading_ovulation = TRUE)
  n <- sc(impute_next_menses = TRUE)
  x <- sc(impute_leading_ovulation = TRUE, impute_next_menses = TRUE)
  for (k in 1:15) {
    expect_equal(cov_cti(x, k),
                 cov_cti(b, k) + (cov_cti(l, k) - cov_cti(b, k)) + (cov_cti(n, k) - cov_cti(b, k)),
                 info = paste("person", k))
  }
})

test_that("person 15 runs both rules at once, additively, fabricating rows at both ends", {
  b <- sc(); l <- sc(impute_leading_ovulation = TRUE); n <- sc(impute_next_menses = TRUE)
  x <- sc(impute_leading_ovulation = TRUE, impute_next_menses = TRUE)
  expect_equal(cov_cti(l, 15) - cov_cti(b, 15), 15L)
  expect_equal(cov_cti(n, 15) - cov_cti(b, 15), 14L)
  expect_equal(cov_cti(x, 15) - cov_cti(b, 15), 29L)      # additive, so the order is moot
  expect_equal(nrow_id(x, 15) - nrow_id(b, 15), 10L)      # 5 at the front, 5 at the back
  # the leading rule reaches only the imputed columns, on this person too
  expect_equal(cov_ct(l, 15) - cov_ct(b, 15), 0L)
  expect_equal(cov_ct(n, 15) - cov_ct(b, 15), 14L)
})

test_that("an imputed first onset never becomes a leading-day anchor (person 14)", {
  # impute_next_menses creates person 14's ONLY onset, from a confirmed
  # ovulation that therefore precedes it -- which is the leading rule's own
  # decline condition. So no setting combination imputes an ovulation there.
  for (x in list(sc(impute_leading_ovulation = TRUE),
                 sc(impute_leading_ovulation = TRUE, impute_next_menses = TRUE))) {
    expect_equal(sum(x$ovtoday_leading_impute[x$id == 14] == 1, na.rm = TRUE), 0L)
  }
})

test_that("leading_ovulation_luteal_days is a placement with no cap and no phase gate", {
  b <- sc()
  for (k in c(5L, 15L, 25L)) {
    expect_equal(cov_cti(sc(impute_leading_ovulation = TRUE,
                            leading_ovulation_luteal_days = k), 3) - cov_cti(b, 3), k,
                 info = paste("leading_ovulation_luteal_days =", k))
  }
  # 25 days exceeds luteal_phase_max_days (18) and is scaled anyway: this path
  # does not consult the phase caps at all
  expect_equal(cov_cti(sc(impute_leading_ovulation = TRUE,
                          leading_ovulation_luteal_days = 25,
                          luteal_phase_max_days = 18), 3) - cov_cti(b, 3), 25L)
  # and past the diary's own start it fabricates rows rather than stopping
  expect_gt(nrow_id(sc(impute_leading_ovulation = TRUE,
                       leading_ovulation_luteal_days = 40), 3), nrow_id(b, 3))
})

test_that("persons 8 and 9 depend on upper_cyclength_bound admitting a 35-day cycle", {
  wide <- sc()                      # default upper bound is 35, their cycle length
  narrow <- sc(upper_cyclength_bound = 34)
  ids <- c(8, 9)
  expect_equal(sum(!is.na(wide$cyclic_time_impute[wide$id %in% ids])), 72L)
  expect_equal(sum(!is.na(narrow$cyclic_time_impute[narrow$id %in% ids])), 24L)
  expect_equal(sum(narrow$cyclic_time_impute_extended_phase[narrow$id %in% ids] == 1,
                   na.rm = TRUE), 0L)
  # cyclic_time itself is untouched by the cycle-length bounds
  expect_equal(sum(!is.na(wide$cyclic_time[wide$id %in% ids])),
               sum(!is.na(narrow$cyclic_time[narrow$id %in% ids])))
})

test_that("the cycle-time axis stays inside its -1/+1 wrap on every case", {
  x <- sc(impute_leading_ovulation = TRUE, impute_next_menses = TRUE)
  for (v in c("cyclic_time", "cyclic_time_impute", "cyclic_time_ov", "cyclic_time_imp_ov")) {
    expect_true(all(x[[v]] >= -1 & x[[v]] <= 1, na.rm = TRUE), info = v)
  }
})

test_that("ovtoday = NULL runs the whole dataset down the imputed path", {
  no_ov <- cycledata_special[, setdiff(names(cycledata_special), "ovtoday")]
  x <- suppressMessages(suppressWarnings(pacts_scaling(
    no_ov, id = id, date = daterated, menses = menses, ovtoday = NULL)))
  expect_equal(sum(!is.na(x$cyclic_time)), 0L)        # nothing confirmed anywhere
  expect_gt(sum(!is.na(x$cyclic_time_impute)), 0L)
  expect_equal(sum(x$ovtoday_impute == 1, na.rm = TRUE), 24L)
})
