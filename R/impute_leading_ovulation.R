#' Impute a leading ovulation anchor before a participant's first recorded menses onset
#'
#' Days observed BEFORE a participant's first recorded menses onset (the left-censored
#' start of participation) have no cycle anchor on their left, so PACTS cannot place them:
#' the luteal phase they belong to has a closing menses (the first onset) but no ovulation.
#' This opt-in rule imputes that missing ovulation with the same population-average
#' backward count the package already uses inside observed cycles: ovulation is placed
#' `luteal_days` days (default 15) before the first menses onset, so the leading days scale
#' as the end of a luteal phase in `cyclic_time_impute` / `cyclic_time_imp_ov` (never in
#' the confirmed-only columns). The anchor is marked in a new column
#' `ovtoday_leading_impute` (1 on the imputed day, 0 elsewhere). If the imputed day lies
#' before the first observed row, a blank row is added on that date (other columns `NA`),
#' exactly as `impute_next_menses_onsets()` adds a blank row for an imputed onset.
#'
#' Nothing is imputed when the participant has no menses onset, when no observed row
#' precedes the first onset, or when a confirmed ovulation (`ovtoday == 1`) already lies
#' before the first onset. Only the general rule is applied; study-specific gating is the
#' caller's responsibility.
#'
#' @param data,id,date,menses,ovtoday As in [pacts_scaling()].
#' @param luteal_days Integer; days before the first menses onset at which the ovulation is
#'   placed. Default 15 (the package's backward-count convention).
#' @return `data` with an added integer column `ovtoday_leading_impute`, and possibly one
#'   added row per participant on the imputed date.
#' @keywords internal
impute_leading_ovulation_anchors <- function(data, id, date, menses, ovtoday, luteal_days = 15) {
  `%>%` <- magrittr::`%>%`
  idn <- rlang::as_name(rlang::enquo(id));    dn <- rlang::as_name(rlang::enquo(date))
  mn  <- rlang::as_name(rlang::enquo(menses)); on <- rlang::as_name(rlang::enquo(ovtoday))
  orig_class <- class(data[[dn]])
  as_date_v  <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x, tz = attr(x, "tzone")))
    lubridate::ymd(x)
  }
  back_class <- function(x) if (identical(orig_class, "Date")) x
                            else if (any(orig_class %in% c("character", "factor"))) as.character(x)
                            else x
  d_date  <- as_date_v(data[[dn]])
  is_ov   <- !is.na(data[[on]]) & data[[on]] == 1
  is_mens <- !is.na(data[[mn]]) & data[[mn]] == 1
  data$ovtoday_leading_impute <- 0L
  if (!any(is_mens)) return(data)
  per <- data.frame(id = data[[idn]], d = d_date, mens = is_mens, ov = is_ov, stringsAsFactors = FALSE) %>%
    dplyr::filter(!is.na(.data$d)) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise(first_obs   = min(.data$d),
                     first_mens  = if (any(.data$mens)) min(.data$d[.data$mens]) else as.Date(NA),
                     ov_before   = any(.data$ov & .data$d < (if (any(.data$mens)) min(.data$d[.data$mens]) else as.Date(NA))),
                     .groups = "drop") %>%
    dplyr::filter(!is.na(.data$first_mens), .data$first_obs < .data$first_mens, !.data$ov_before) %>%
    dplyr::mutate(cand_date = .data$first_mens - lubridate::days(luteal_days))
  if (nrow(per) == 0) return(data)
  key      <- paste(data[[idn]], as.character(d_date))
  cand_key <- paste(per$id, as.character(per$cand_date))
  hit <- key %in% cand_key & !is_mens & !is_ov
  data$ovtoday_leading_impute[hit] <- 1L
  new <- per %>% dplyr::filter(!(paste(.data$id, as.character(.data$cand_date)) %in% key))
  if (nrow(new) > 0) {
    add <- data[rep(1, nrow(new)), , drop = FALSE]
    add[] <- lapply(add, function(x) x[NA_integer_])
    add[[idn]] <- new$id
    add[[dn]]  <- back_class(new$cand_date)
    add[[mn]]  <- 0
    add[[on]]  <- 0
    add$ovtoday_leading_impute <- 1L
    data <- dplyr::bind_rows(data, add)
  }
  data
}
