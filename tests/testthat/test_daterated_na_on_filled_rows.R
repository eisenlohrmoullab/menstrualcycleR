# Regression test for the "original date column left NA on filled rows" bug.
#
# Cause: calculate_mcyclength() densifies the calendar with
# tidyr::complete(date = seq.Date(...)). On every fabricated row (a missing
# calendar day) the canonical `date` column is filled, but the user's ORIGINAL
# date column (e.g. `daterated`) is left NA. A consumer joining results back by
# the original date name then silently drops any imputed-ovulation / scaled row
# that landed on a filled day.
#
# Invariant under test: every returned date column is internally consistent --
# no NA in the user's original date column on a row that has a real calendar
# `date` or carries a real scaled/cycle value.

# Three ~28-day menses-to-menses cycles with NO observed ovulation (forces
# imputation), and GAPS on the imputed-ovulation days (global days 14/42/70, =
# 28 - 14 luteal). Those gaps are exactly the rows tidyr::complete() fabricates,
# so the imputed ovulation lands on an NA-`daterated` row pre-fix.
make_gappy_subject <- function(id = "S01", start = "2024-01-01",
                               onsets = c(1, 29, 57, 85),
                               drop_days = c(14, 42, 70)) {
  days  <- seq.Date(as.Date(start), as.Date(start) + max(onsets) - 1, by = "day")
  dayno <- seq_along(days)
  d <- data.frame(
    id        = id,
    daterated = days,
    menses    = as.integer(dayno %in% onsets),
    ovtoday   = 0L,
    stringsAsFactors = FALSE
  )
  d[!(dayno %in% drop_days), , drop = FALSE]
}

scale_quiet <- function(df) {
  suppressMessages(
    pacts_scaling(df, id = id, date = daterated, menses = menses, ovtoday = ovtoday)
  )
}

test_that("the user's original date column is never NA where a real `date` exists", {
  res <- scale_quiet(make_gappy_subject())

  expect_true("date" %in% names(res))
  expect_true("daterated" %in% names(res))

  # No row should have a real canonical date but a missing original date.
  expect_equal(sum(!is.na(res$date) & is.na(res$daterated)), 0L)

  # Where both are present they must agree (the original is not rewritten).
  expect_true(all(res$date == res$daterated, na.rm = TRUE))
})

test_that("scaled / cycle values never sit on an NA original-date row", {
  res <- scale_quiet(make_gappy_subject())

  value_cols <- c("scaled_cycleday", "scaled_cycleday_impute",
                  "scaled_cycleday_ov", "scaled_cycleday_imp_ov",
                  "cyclic_time", "cyclic_time_impute",
                  "cyclic_time_ov", "cyclic_time_imp_ov")
  value_cols <- intersect(value_cols, names(res))

  has_value <- Reduce(`|`, lapply(value_cols, function(cn) !is.na(res[[cn]])))
  expect_equal(sum(has_value & is.na(res$daterated)), 0L)

  # The imputed-ovulation rows specifically must carry a valid original date.
  ov_imp <- res[res$ovtoday_impute == 1, , drop = FALSE]
  expect_gt(nrow(ov_imp), 0L)
  expect_false(any(is.na(ov_imp$daterated)))
})

test_that("passing a date column literally named `date` does not error or duplicate", {
  df <- make_gappy_subject()
  names(df)[names(df) == "daterated"] <- "date"

  res <- suppressMessages(
    pacts_scaling(df, id = id, date = date, menses = menses, ovtoday = ovtoday)
  )
  expect_equal(sum(names(res) == "date"), 1L)   # no duplicate date column
  expect_false(any(is.na(res$date)))
})

# --- Date-column TYPE variants ------------------------------------------------
# character / factor original date columns are a SUPPORTED input
# (calculate_mcyclength coerces them with lubridate::ymd). A naive
# coalesce(!!date, date) crashes on them ("Can't combine <character>/<factor>
# and <date>") because the internal canonical `date` is Date-class while the
# original keeps its input type. The fix coerces character/factor originals to
# Date before coalescing. POSIXct and Date originals coalesce directly and keep
# their own type. These cases lock in that the fix runs to completion AND still
# fills NA fabricated rows for every supported input type.

test_that("character original date column does not crash and is fully filled", {
  df <- make_gappy_subject()
  df$daterated <- as.character(df$daterated)

  res <- scale_quiet(df)
  expect_true("daterated" %in% names(res))
  # No row with a real canonical `date` but an NA original date.
  expect_equal(sum(!is.na(res$date) & is.na(res$daterated)), 0L)
  # Coerced to Date (the package's own internal coercion); values agree.
  expect_s3_class(res$daterated, "Date")
  expect_true(all(res$date == res$daterated, na.rm = TRUE))
})

test_that("factor original date column does not crash and is fully filled", {
  df <- make_gappy_subject()
  df$daterated <- factor(as.character(df$daterated))

  res <- scale_quiet(df)
  expect_equal(sum(!is.na(res$date) & is.na(res$daterated)), 0L)
  expect_s3_class(res$daterated, "Date")
  expect_true(all(res$date == res$daterated, na.rm = TRUE))
})

test_that("POSIXct original date column is filled and retains its type", {
  df <- make_gappy_subject()
  df$daterated <- as.POSIXct(df$daterated, tz = "UTC")

  res <- scale_quiet(df)
  expect_equal(sum(!is.na(res$date) & is.na(res$daterated)), 0L)
  # POSIXct originals keep their own type (only character/factor are coerced).
  expect_s3_class(res$daterated, "POSIXct")
  expect_true(all(res$date == as.Date(res$daterated), na.rm = TRUE))
})

# --- Original ID column on fabricated rows (sibling of the date bug) -----------
# The densify groups by the canonical `id` and completes on the canonical `date`,
# so BOTH canonical keys stay populated on fabricated rows (id via the group key +
# fill(), date via complete()). But the user's ORIGINAL id column, when it is not
# literally named `id` (e.g. `record_id`, `subject`), rides along as a passenger
# and is left NA on fabricated rows -- exactly like the original date column. A
# downstream join on id + date then drops those rows even after the date is fixed.
# The canonical `id` is a plain copy of the original (no coercion), so the refill
# needs no type cast and preserves the input type. These cases lock that in.

# Same gappy single subject, but with the id column under an arbitrary name.
make_gappy_named_id <- function(id_col = "record_id", id_val = "S01",
                                start = "2024-01-01",
                                onsets = c(1, 29, 57, 85),
                                drop_days = c(14, 42, 70)) {
  days  <- seq.Date(as.Date(start), as.Date(start) + max(onsets) - 1, by = "day")
  dayno <- seq_along(days)
  d <- data.frame(
    record_id = id_val,
    daterated = days,
    menses    = as.integer(dayno %in% onsets),
    ovtoday   = 0L,
    stringsAsFactors = FALSE
  )
  names(d)[names(d) == "record_id"] <- id_col
  d[!(dayno %in% drop_days), , drop = FALSE]
}

test_that("a differently-named original id column is never NA on fabricated rows", {
  df  <- make_gappy_named_id(id_col = "record_id", id_val = "S01")
  res <- suppressMessages(
    pacts_scaling(df, id = record_id, date = daterated,
                  menses = menses, ovtoday = ovtoday)
  )
  expect_true("record_id" %in% names(res))

  # No row should have a real canonical date/id but a missing original id.
  expect_equal(sum(!is.na(res$date) & is.na(res$record_id)), 0L)
  # Character id keeps its type (no coercion), and value is filled, not changed.
  expect_type(res$record_id, "character")
  expect_equal(unique(res$record_id), "S01")

  # Scaled / cycle values (incl. imputed-ov rows) never sit on an NA original-id row.
  value_cols <- intersect(
    c("scaled_cycleday", "scaled_cycleday_impute", "scaled_cycleday_ov",
      "scaled_cycleday_imp_ov", "cyclic_time", "cyclic_time_impute",
      "cyclic_time_ov", "cyclic_time_imp_ov"),
    names(res)
  )
  has_value <- Reduce(`|`, lapply(value_cols, function(cn) !is.na(res[[cn]])))
  expect_equal(sum(has_value & is.na(res$record_id)), 0L)
  expect_false(any(is.na(res[res$ovtoday_impute == 1, ]$record_id)))
})

test_that("an integer original id column is filled and retains its type", {
  df  <- make_gappy_named_id(id_col = "record_id", id_val = 7L)
  res <- suppressMessages(
    pacts_scaling(df, id = record_id, date = daterated,
                  menses = menses, ovtoday = ovtoday)
  )
  expect_equal(sum(!is.na(res$date) & is.na(res$record_id)), 0L)
  expect_type(res$record_id, "integer")
  expect_equal(unique(res$record_id), 7L)
})

test_that("passing an id column literally named `id` is not duplicated or NA-filled wrong", {
  df <- make_gappy_subject()   # id column is already named `id`
  res <- scale_quiet(df)
  expect_equal(sum(names(res) == "id"), 1L)   # no duplicate id column
  expect_false(any(is.na(res$id)))
})

test_that("an observed original date is never overwritten by `date` (NA-fill only)", {
  # No gaps -> no fabricated rows -> coalesce must be a strict no-op on the
  # original column (value-preservation invariant).
  days  <- seq.Date(as.Date("2024-01-01"), as.Date("2024-03-25"), by = "day")
  dayno <- seq_along(days)
  onsets <- c(1, 29, 57, 85)
  df <- data.frame(
    id        = "S01",
    daterated = days,
    menses    = as.integer(dayno %in% onsets),
    ovtoday   = 0L,
    stringsAsFactors = FALSE
  )
  res <- scale_quiet(df)
  # Every observed original date round-trips unchanged.
  expect_equal(sum(!is.na(res$date) & res$date != res$daterated), 0L)
  expect_false(any(is.na(res$daterated)))
})
