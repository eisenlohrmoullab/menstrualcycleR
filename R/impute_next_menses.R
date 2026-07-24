#' Impute a next-menses onset from a confirmed ovulation (Internal)
#'
#' Implements the GENERAL next-menses imputation rule: for each
#' biomarker-confirmed ovulation (`ovtoday == 1`) that is not already closed by
#' an observed menses onset within `max_window` days, synthesize a menses onset
#' at `ovulation + luteal_days` (the population-average luteal length, so
#' ovulation + 14 = the "LH+15"/last-follicular-day rule). This lets a confirmed
#' ovulation whose next menses was never recorded still contribute a scalable
#' cycle, rather than being dropped for lack of a closing anchor.
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
#' @param max_window Skip imputation for an ovulation if an observed menses onset
#'   falls within this many days after it. Default 20.
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

  ov  <- tibble::tibble(id = data[[idn]][is_ov],   ov_date = d_date[is_ov])
  men <- tibble::tibble(id = data[[idn]][is_mens], m_date  = d_date[is_mens])

  # candidate onset = ov + luteal_days; drop it if an observed menses closes the
  # cycle within (ov, ov + max_window]
  cand <- ov %>%
    dplyr::mutate(cand_date = .data$ov_date + lubridate::days(luteal_days)) %>%
    dplyr::left_join(men, by = "id", relationship = "many-to-many") %>%
    dplyr::group_by(.data$id, .data$ov_date, .data$cand_date) %>%
    dplyr::summarise(has_closing = any(!is.na(.data$m_date) &
                                       .data$m_date >  .data$ov_date &
                                       .data$m_date <= .data$ov_date + lubridate::days(max_window)),
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
