#' Impute a next-menses onset from a confirmed ovulation (Internal)
#'
#' Implements the GENERAL next-menses imputation rule: for each
#' biomarker-confirmed ovulation (`ovtoday == 1`) that has NO observed menses
#' onset recorded anywhere after it for that person, synthesize a menses onset
#' at `ovulation + luteal_days` (the population-average luteal length, so
#' ovulation + 14 = the "LH+15"/last-follicular-day rule). This lets a confirmed
#' ovulation whose next menses was never recorded at all still contribute a
#' scalable cycle, rather than being dropped for lack of a closing anchor.
#'
#' A real, observed closing menses -- at ANY distance from the ovulation, not
#' just a nearby one -- always wins and is never overridden or duplicated by an
#' imputed onset (see the `max_window` entry below for why this search is
#' deliberately unbounded as of 0.1.7).
#'
#' Study-specific gating (e.g. blocking imputation across treatment phases or
#' documented off-study breaks, trust-ordered de-duplication of anchors) is
#' deliberately NOT done here; it is the caller's responsibility to apply on top.
#' This function does the general rule only.
#'
#' Operates on the user's own `id` / `date` / `menses` / `ovtoday` columns and is
#' run before cycle-length calculation, so downstream scaling sees the augmented
#' anchors. Imputed onsets are marked in a new `menses_impute` column (`1` =
#' imputed onset, `0` = everything else).
#'
#' @param data A data frame in long format (one row per id-date).
#' @param id,date,menses,ovtoday Unquoted column names, as in [pacts_scaling()].
#' @param luteal_days Days after ovulation to place the imputed onset. Default 14.
#' @param max_window Not currently used to gate the impute decision -- kept for
#'   argument-signature stability; a non-default value has no effect on the
#'   result. Through 0.1.6, imputation fired whenever no OBSERVED menses onset
#'   fell within `max_window` days after a confirmed ovulation (default 20).
#'   That was a real bug, not a documented tradeoff: a genuine luteal phase
#'   just outside the window (confirmed on this package's own `cycledata`
#'   example, id 8: a real 22-day luteal phase) got a fabricated onset
#'   synthesized at day 14 anyway, which OVERWROTE that participant's actually
#'   observed `menses == 0` day to `1` -- corrupting real data, not just adding
#'   an imputed row. As of 0.1.7 a real closing menses at ANY distance always
#'   prevents imputation; only a confirmed ovulation with NO recorded closing
#'   menses anywhere in that person's remaining data is treated as open. See
#'   `NEWS.md` for the full history.
#'
#' @return `data` with imputed menses onsets added (existing rows updated, or new
#'   rows appended for onset dates outside the observed range) and a
#'   `menses_impute` indicator column.
#' @keywords internal
impute_next_menses_onsets <- function(data, id, date, menses, ovtoday,
                                      luteal_days = 14, max_window = 20) {
  `%>%` <- magrittr::`%>%`
  idn <- rlang::as_name(rlang::enquo(id));    dn <- rlang::as_name(rlang::enquo(date))
  mn  <- rlang::as_name(rlang::enquo(menses)); on <- rlang::as_name(rlang::enquo(ovtoday))

  orig_class <- class(data[[dn]])
  as_date_v  <- function(x) if (inherits(x, "Date")) x else lubridate::ymd(x)
  # Return imputed onset dates in the SAME class the caller passed, so bind_rows
  # does not error on a <Date> vs <character>/<factor> mismatch and the downstream
  # lubridate::ymd() coercion behaves identically to the observed rows.
  back_class <- function(x) if (identical(orig_class, "Date")) x
                            else if (any(orig_class %in% c("character", "factor"))) as.character(x)
                            else x

  d_date  <- as_date_v(data[[dn]])
  is_ov   <- !is.na(data[[on]]) & data[[on]] == 1
  is_mens <- !is.na(data[[mn]]) & data[[mn]] == 1
  if (!any(is_ov)) { data$menses_impute <- 0L; return(data) }

  # next_ov_date: the id's own next confirmed ovulation after this one (NA if
  # there isn't one). Needed below to scope "closing menses" to THIS
  # ovulation's cycle -- without it, a real closing menses that actually
  # belongs to a LATER ovulation gets misread as closing this one too,
  # wrongly suppressing imputation for a genuinely open earlier cycle in any
  # id with more than one confirmed ovulation.
  ov  <- data.frame(id = data[[idn]][is_ov],   ov_date = d_date[is_ov],   stringsAsFactors = FALSE) %>%
    dplyr::arrange(.data$id, .data$ov_date) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::mutate(next_ov_date = dplyr::lead(.data$ov_date)) %>%
    dplyr::ungroup()
  men <- data.frame(id = data[[idn]][is_mens], m_date  = d_date[is_mens], stringsAsFactors = FALSE)

  # candidate onset = ov + luteal_days; drop it if any observed menses closes
  # THIS ovulation's own cycle -- falls after ov_date and, if there is a next
  # confirmed ovulation for this id, strictly before it. Deliberately NOT
  # restricted to max_window (see the roxygen entry for max_window: bounding
  # this search by a day count was the 0.1.6 bug, not a real design tradeoff.
  # A real closing menses recorded at any distance is always the right
  # anchor; synthesizing an earlier phantom one when a real later one exists
  # would overwrite actual data with fabricated data, which is strictly worse
  # than leaving a long real cycle for the caller's overall cycle-length
  # bound to exclude if it's implausibly long). The next-ovulation bound
  # above is a different kind of restriction -- it scopes "closing" to the
  # right cycle, it does not reintroduce a day-count cap.
  cand <- ov %>%
    dplyr::mutate(cand_date = .data$ov_date + lubridate::days(luteal_days)) %>%
    dplyr::left_join(men, by = "id", relationship = "many-to-many") %>%
    dplyr::group_by(.data$id, .data$ov_date, .data$cand_date) %>%
    dplyr::summarise(has_closing = any(!is.na(.data$m_date) & .data$m_date > .data$ov_date &
                                          (is.na(.data$next_ov_date) | .data$m_date < .data$next_ov_date)),
                     .groups = "drop") %>%
    dplyr::filter(!.data$has_closing) %>%
    dplyr::distinct(.data$id, .data$cand_date)

  data$menses_impute <- 0L
  if (nrow(cand) == 0) return(data)

  key      <- paste(data[[idn]], as.character(d_date))
  cand_key <- paste(cand$id,     as.character(cand$cand_date))
  hit <- key %in% cand_key & !is_mens          # existing rows on an imputed-onset date
  data[[mn]][hit] <- 1
  data$menses_impute[hit] <- 1L

  new <- cand %>% dplyr::filter(!(paste(.data$id, as.character(.data$cand_date)) %in% key))
  if (nrow(new) > 0) {
    add <- data[rep(1, nrow(new)), , drop = FALSE]
    add[] <- lapply(add, function(x) x[NA_integer_])   # blank template rows
    add[[idn]] <- new$id
    add[[dn]]  <- back_class(new$cand_date)
    add[[mn]]  <- 1
    add[[on]]  <- 0
    add$menses_impute <- 1L
    data <- dplyr::bind_rows(data, add)
  }
  data[order(data[[idn]], as_date_v(data[[dn]])), , drop = FALSE]
}
