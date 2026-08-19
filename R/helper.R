#' Process Data (Internal)
#'
#' Internal helper functions to process the data.
#'
#' @param data A data frame containing cycle data.
#' @param id A column specifying individual ids.
#' @param date A column specifying the dates.
#' @param menses A column indicating menses (0/1).
#'
#' @return A data frame with processed data.
#' @keywords internal
#' 
#' 

process_luteal_phase_base <- function(data, id, date, menses,
                                       luteal_phase_min_days = 7,
                                       luteal_phase_max_days = 18) {
  `%>%` <- magrittr::`%>%`
  
  # Quote column names for tidy evaluation
  id <- rlang::enquo(id)
  date <- rlang::enquo(date)
  menses <- rlang::enquo(menses)
  
  # Group and arrange data
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::arrange(!!date, .by_group = TRUE) %>%
    dplyr::mutate(lutmax = NA)
  
  # Helper function to calculate lutdaycount1
  calculate_lutdaycount <- function(data, id_col, menses_col) {
    last_id <- NULL
    lutdaycount1 <- rep(NA, nrow(data))
    
    for (i in 1:nrow(data)) {
      if (is.null(last_id) || last_id != data[[id_col]][i]) {
        lutdaycount1[i] <- ifelse(data$ovtoday[i] == 1, 0, NA)
      } else if (!is.na(lutdaycount1[i - 1])) {
        lutdaycount1[i] <- lutdaycount1[i - 1] + 1
      }
      
      if (!is.na(lutdaycount1[i]) &&
          !is.na(data[[menses_col]][i]) && data[[menses_col]][i] == 1) {
        lutdaycount1[i] <- NA
      } else if (data$ovtoday[i] == 1) {
        lutdaycount1[i] <- 0
      }
      
      last_id <- data[[id_col]][i]
    }
    
    data$lutdaycount1 <- lutdaycount1
    return(data)
  }
  
  # Apply the helper function to calculate lutdaycount1
  data <- calculate_lutdaycount(
    data,
    id_col = rlang::quo_name(id),
    menses_col = rlang::quo_name(menses)
  )
  
  # Calculate lutdaycount
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::mutate(
      lutdaycount = dplyr::lag(lutdaycount1),
      lutdaycount = dplyr::case_when(
        is.na(lutdaycount) | !!id != dplyr::lag(!!id) ~ NA,
        TRUE ~ lutdaycount
      )
    )
  
  # Calculate lutmax
  # A luteal run ends either when the next row's lutdaycount is NA or when we
  # reach the final row of the dataset. The `i == nrow(data)` clause closes the
  # run for the last participant; without it, the last row was never treated as
  # a run-end, so the final participant's luteal phase never received a lutmax
  # and scaled_cycleday came back all-NA (position-dependent bug).
  for (i in 1:nrow(data)) {
    if (!is.na(data$lutdaycount[i]) &&
        (i == nrow(data) || is.na(data$lutdaycount[i + 1]))) {
      data$lutmax[(i - (data$lutdaycount[i])):i] <- as.numeric(data$lutdaycount[i])
    }
  }
  
  # Calculate lutperc and lutperc1
  data <- data %>%
    dplyr::mutate(
      lutperc = ifelse(lutmax <= luteal_phase_max_days & lutmax >= luteal_phase_min_days, lutdaycount / lutmax, NA),
      lutperc1 = lutperc - 1
    )

  # Calculate cyclic time luteal phase part
  data$cyclic_lut1 = data$lutdaycount1/(data$lutmax+1)

  data = data %>%
    dplyr::mutate(
      cyclic_lut = dplyr::case_when(
        lutmax <= luteal_phase_max_days &
          lutmax >= luteal_phase_min_days ~ (-1 * (1 - cyclic_lut1)),
        TRUE ~ NA
      )
    )

  # Uncapped luteal cyclic-time -- same math as cyclic_lut above, minus the
  # luteal_phase_max_days upper cap, gated instead on the cycle staying within
  # the caller's own [lower_cyclength_bound, upper_cyclength_bound]. Feeds
  # ONLY cyclic_time_impute's last-resort fallback in create_scaled_cycleday();
  # cyclic_lut / cyclic_time (the confirmed-only, strict columns) are computed
  # above and never read this. See create_scaled_cycleday() for why the
  # fallback is needed: a confirmed ovulation whose luteal run exceeds
  # luteal_phase_max_days gets no coverage from cyclic_lut, and none from the
  # imputed-ovulation path either, because calculate_ovtoday_impute()
  # correctly suppresses imputed ovulation once a confirmed one already
  # exists in the cycle. A caller who wants MORE luteal coverage in
  # cyclic_time itself should widen luteal_phase_max_days directly rather
  # than lean on this fallback -- this exists for whatever still runs past
  # even a caller-chosen phase bound, up to the overall cycle-length bound.
  #
  # Requires cycle_incomplete != 1, matching every other mcyclength-gated
  # column in this file (follength_impute, lutperc_impute, lutperc_imp_ov,
  # percfol_impute below) -- mcyclength on an incomplete/still-open trailing
  # cycle is just "however many days were observed so far"
  # (calculate_mcyclength.r), not a true closed cycle length, so it can land
  # inside the caller's bounds by coincidence of when data collection ended.
  # Without this gate, a right-censored confirmed-ovulation cycle could get
  # fabricated cyclic_time_impute coverage for days that were never actually
  # closed by a real next menses.
  data = data %>%
    dplyr::mutate(
      cyclic_lut_uncapped = dplyr::case_when(
        lutmax >= luteal_phase_min_days &
          cycle_incomplete != 1 &
          mcyclength >= lower_cyclength_bound &
          mcyclength <= upper_cyclength_bound ~ (-1 * (1 - cyclic_lut1)),
        TRUE ~ NA
      )
    )

  # Calculate ovulation-centered luteal phase part of cyclic time
  data = data %>%
    dplyr::mutate(cyclic_lut_ov = dplyr::case_when(!is.na(cyclic_lut) ~ 1 + cyclic_lut,
                                                   TRUE ~ NA))

  # Ovulation-centered counterpart of cyclic_lut_uncapped -- same derivation
  # as cyclic_lut_ov above, applied to the uncapped value. Feeds only
  # cyclic_time_imp_ov's extended-phase fallback in create_scaled_cycleday();
  # cyclic_lut_ov / cyclic_time_ov are computed above and never read this.
  data = data %>%
    dplyr::mutate(cyclic_lut_ov_uncapped = dplyr::case_when(!is.na(cyclic_lut_uncapped) ~ 1 + cyclic_lut_uncapped,
                                                            TRUE ~ NA))

  # Luteal phase length variable
  data = data %>%
    dplyr::mutate(
      luteal_length = dplyr::case_when(
        lutmax <= luteal_phase_max_days &
          lutmax >= luteal_phase_min_days ~ lutmax,
        TRUE ~ NA
      )
    )

  #NOTE: we only scale luteal phases if the length is <= luteal_phase_max_days and >= luteal_phase_min_days (defaults 18 and 7). This judgement is based on norms from Bull et al., 2019 in 21 to 35 day cycles; both bounds are caller-adjustable (see pacts_scaling()'s "Internal phase-length caps" section). Scaling is NOT based on mcyclength (menses-to-menses cycle length), because then of someone only had a luteal phase    in study, bookended by ovulation and menses, it would not get scaled. Or if they had 1 luteal phase and then a full menses-to-menses cycle   the luteal phase would not get scaled.
  
  data = data %>%
    dplyr::mutate(luteal_length = dplyr::case_when(is.na(lutdaycount1) ~ NA, TRUE ~ luteal_length))
  
  # Calculate lutdaycount_ov and lutperc_ov
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::mutate(
      lutdaycount_ov = dplyr::lead(lutdaycount),
      lutdaycount_ov = dplyr::case_when(
        is.na(lutdaycount_ov) | !!id != dplyr::lead(!!id) ~ NA,
        TRUE ~ lutdaycount_ov
      ),
      next_id = dplyr::lead(!!id),  # Capture the ID of the next row
      valid_group = any(lutdaycount_ov == lutmax & id == next_id),
      lutperc_ov = ifelse(lutmax <= luteal_phase_max_days & lutmax >= luteal_phase_min_days & valid_group, lutdaycount_ov / lutmax, NA),
      lutperc_ov = ifelse(lutdaycount_ov == 0, 0, lutperc_ov)
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::select(-next_id, -valid_group)
  
  return(data)
}


process_follicular_phase_base <- function(data, id, date, menses,
                                           follicular_phase_min_days = 8,
                                           follicular_phase_max_days = 25) {
  `%>%` <- magrittr::`%>%`
  
  # Early validation for required columns
  required_columns <- c("id", "menses", "ovtoday")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(glue::glue(
      "Input data must include the following missing columns: {paste(missing_columns, collapse = ', ')}."
    ))
  }
  
  if (nrow(data) == 0) {
    stop("Input data is empty.")
  }
  
  # Quote column names for tidy evaluation
  id <- rlang::enquo(id)
  date <- rlang::enquo(date)
  menses <- rlang::enquo(menses)
  
  # Processing logic remains unchanged
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::arrange(!!date, .by_group = TRUE) %>%
    dplyr::mutate(folmax = NA, foldaycount = NA)
  
  # Function to calculate foldaycount
  calculate_foldaycount <- function(data, id_col, menses_col) {
    foldaycount <- NA
    last_id <- NULL
    data[[menses_col]][is.na(data[[menses_col]])] <- 0
    
    for (i in seq_len(nrow(data))) {
      if (is.null(last_id) || last_id != data[[id_col]][i]) {
        # Restart counting when ID changes
        foldaycount <- ifelse(data[[menses_col]][i] == 1, 0, NA)
      } else if (!is.na(foldaycount)) {
        foldaycount <- foldaycount + 1
      }
      if (
        !is.na(foldaycount) &&
        i >= 3 &&
        !is.na(data$ovtoday[i]) &&
        i > 1 && !is.na(data$ovtoday[i - 1]) &&
        data$ovtoday[i - 1] == 1
      ) {
        # Stop counting one row after ovtoday == 1
        foldaycount <- NA
      } else if (data[[menses_col]][i] == 1) {
        # Start counting when menses == 1
        foldaycount <- 0
      }
      # Assign foldaycount to current row
      data$foldaycount[i] <- foldaycount
      last_id <- data[[id_col]][i]
    }
    
    return(data)
  }
  
  # Apply the helper function
  data <- calculate_foldaycount(
    data,
    id_col = rlang::quo_name(id),
    menses_col = rlang::quo_name(menses)
  )
  
  # Calculate folmax = follicular phase length
  # As with lutmax above, a run also ends at the final row of the dataset, so
  # the last participant's follicular phase is closed via the `i == nrow(data)`
  # clause rather than being silently dropped.
  for (i in 1:nrow(data)) {
    if (!is.na(data$foldaycount[i]) &&
        (i == nrow(data) || is.na(data$foldaycount[i + 1]))) {
      data$folmax[(i - (data$foldaycount[i])):i] <- as.numeric(data$foldaycount[i])
    }
  }
  
  # Calculate follength = folmax + 1 (to include menses onset)
  data <- data %>%
    dplyr::mutate(follength = folmax + 1)
  
  # Calculate folperc (only when follicular length is within [follicular_phase_min_days, follicular_phase_max_days])
  #NOTE: we only scale follicular phases if the length is <= follicular_phase_max_days and >= follicular_phase_min_days (defaults 25 and 8). This judgement is based on norms from Bull et al., 2019 in 21 to 35 day cycles; both bounds are caller-adjustable (see pacts_scaling()'s "Internal phase-length caps" section). Scaling is NOT based on mcyclength (menses-to-menses cycle length), because then if someone only had a follicular phase in study, bookended by menses and ovulation, it would not get scaled. Or if they had a full menses-to-menses cycle and then a follicualr phase (bookend by menses and ovulation), the follicular phase would not get scaled.

  data <- data %>%
    dplyr::arrange(!!id, cyclenum,!!date) %>%  # Ensure correct order
    dplyr::group_by(!!id, cyclenum) %>%
    dplyr::mutate(
      next_id = dplyr::lead(!!id),  # Capture the ID of the next row
      valid_group = any(foldaycount == folmax & !!id == next_id),  # Check if the condition is met for the group
      folperc = ifelse(
        follength >= follicular_phase_min_days & follength <= follicular_phase_max_days & valid_group,
        foldaycount / folmax,
        NA_real_
      ),
      # Uncapped follicular fraction -- same run, minus the follicular_phase_max_days
      # upper cap, gated instead on the cycle staying within the caller's own
      # [lower_cyclength_bound, upper_cyclength_bound]. Mirrors
      # cyclic_lut_uncapped above (see its comment for why cycle_incomplete
      # != 1 is required too); feeds only cyclic_time_impute's fallback. A
      # caller who wants more follicular coverage in cyclic_time itself
      # should widen follicular_phase_max_days directly.
      folperc_uncapped = ifelse(
        follength >= follicular_phase_min_days & valid_group & cycle_incomplete != 1 &
          mcyclength >= lower_cyclength_bound & mcyclength <= upper_cyclength_bound,
        foldaycount / folmax,
        NA_real_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-next_id, -valid_group)
  
  
  # Calculate percfol and percfol_ov
  data <- data %>% 
    dplyr::mutate(
      percfol = dplyr::case_when(
        !is.na(lutperc1) & !is.na(folperc) & folperc != 0 ~ NA,
        TRUE ~ folperc
      ),
      percfol_ov = percfol - 1
    )
  
  # Calculate follicular phase part of cyclic time
  
  data = data %>%
    dplyr::mutate(
      cyclic_fol = folperc,
      cyclic_fol_uncapped = folperc_uncapped
    )


  # Calculate ovulation-centered follicular phase part of cyclic time
  data = data %>%
    dplyr::mutate(cyclic_fol_ov = dplyr::case_when(!is.na(cyclic_fol) & !is.na(percfol_ov) ~ -1*(1 - cyclic_fol),
                                                   TRUE ~ NA))

  # Ovulation-centered counterpart of cyclic_fol_uncapped -- mirrors
  # cyclic_lut_ov_uncapped in process_luteal_phase_base(). Feeds only
  # cyclic_time_imp_ov's extended-phase fallback. Carries the same
  # !is.na(percfol_ov) guard as cyclic_fol_ov above -- without it, a
  # luteal/follicular boundary day that percfol_ov nulls out to avoid
  # double-counting (see percfol's own case_when) could still leak an
  # uncapped follicular value into cyclic_time_ov_uncapped with no
  # equivalent dedup protection.
  data = data %>%
    dplyr::mutate(cyclic_fol_ov_uncapped = dplyr::case_when(!is.na(cyclic_fol_uncapped) & !is.na(percfol_ov) ~ -1*(1 - cyclic_fol_uncapped),
                                                             TRUE ~ NA))

  return(data)
}


calculate_ovtoday_impute <- function(data, id, date, menses) {
  `%>%` <- magrittr::`%>%`
  
  # Dynamically handle input column names
  id <- rlang::enquo(id)
  date <- rlang::enquo(date)
  menses <- rlang::enquo(menses)
  
  # Step 1: Calculate `lutlength_impute` and `follength_impute`
  data <- data %>%
    dplyr::mutate(
      lutlength_impute = 14,
      # Assign fixed value 14 to lutlength_impute
      follength_impute = dplyr::if_else(
        cycle_incomplete != 1 &
          (
            mcyclength >= lower_cyclength_bound &
              mcyclength <= upper_cyclength_bound
          ),
        #we do not want to impute ovulation if the cycle is not menses-to-menses bounded in the study and within the lower and upper cycle length bounds (default and recommended and vlaidated at 21-35 days)
        mcyclength - 14,
        NA_real_
      )
    )
  
  # Step 2: Group by `id` and sort by `date`
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::arrange(!!date, .by_group = TRUE)
  
  # Step 3: Calculate `follcount1_impute`
  data$follcount1_impute <- NA
  for (i in 1:nrow(data)) {
    if (data[[rlang::as_name(menses)]][i] == 1 &
        !is.na(data$follength_impute[i])) {
      follcount1_impute <- seq_len(round(data$follength_impute[i]))
      end_index <- i + length(follcount1_impute) - 1
      if (end_index <= nrow(data)) {
        data$follcount1_impute[i:end_index] <- follcount1_impute
      } else {
        data$follcount1_impute[i:nrow(data)] <- follcount1_impute[1:(nrow(data) - i + 1)]
      }
    }
  }
  
  # Step 4: Calculate `ovtoday_impute`
  data <- data %>%
    dplyr::mutate(
      ovtoday_impute = dplyr::case_when(
        !is.na(follength_impute) & !is.na(follcount1_impute) & follength_impute == follcount1_impute ~ 1L,
        TRUE ~ 0L
      )
    )
  
  
  # Step 5: Replace NA values in `ovtoday_impute` with 0
  data$ovtoday_impute <- ifelse(is.na(data$ovtoday_impute), 0, data$ovtoday_impute)
  
  
  # # # Step 6: If ovtoday and ovtoday_impute are within 7 days of each other OR if ovtoday and ovtoday_impute are the same cycle,   ovtoday_impute == 0;
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::arrange(!!date, .by_group = TRUE) %>%
    dplyr::mutate(
      ovtoday_impute = dplyr::if_else(
        ovtoday_impute == 1 & (
          purrr::map_lgl(date, ~ any(abs(difftime(date[ovtoday == 1], .x, units = "days")) <= 7)) |
            cyclenum %in% cyclenum[ovtoday == 1]
        ),
        0L,
        ovtoday_impute
      )
    ) %>%
    dplyr::ungroup()
  
  return(data)
}

process_luteal_phase_impute <- function(data, id, date, menses) {
  `%>%` <- magrittr::`%>%`
  
  # Dynamically handle input column names
  id <- rlang::enquo(id)
  date <- rlang::enquo(date)
  menses <- rlang::enquo(menses)
  
  # Group by ID and arrange by date
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::arrange(!!date, .by_group = TRUE) %>%
    dplyr::mutate(lutmax_impute = NA)
  
  # Helper function to calculate `lutdaycount_impute` and `lutdaycount1_impute`
  calculate_lutdaycount_impute <- function(data, id_col, date_col, ovulation_col, menses_col) {
    id_col <- rlang::as_name(rlang::enquo(id_col))
    date_col <- rlang::as_name(rlang::enquo(date_col))
    ovulation_col <- rlang::as_name(rlang::enquo(ovulation_col))
    menses_col <- rlang::as_name(rlang::enquo(menses_col))
    
    data <- data %>%
      dplyr::arrange(.data[[id_col]], .data[[date_col]]) %>%
      dplyr::group_by(.data[[id_col]]) %>%
      dplyr::mutate(lutdaycount_impute = NA_integer_) %>%
      dplyr::group_modify(~ {
        df <- .x
        ov_idx <- which(df[[ovulation_col]] == 1)
        
        for (i in ov_idx) {
          # Start on the next day after ovtoday_impute == 1
          j <- i + 1
          count <- 0
          while (j <= nrow(df)) {
            df$lutdaycount_impute[j] <- count
            
            # Stop *after* assigning if menses == 1
            if (!is.na(df[[menses_col]][j]) && df[[menses_col]][j] == 1) {
              break
            }
            
            count <- count + 1
            j <- j + 1
          }
        }
        return(df)
      }) %>%
      dplyr::ungroup()
    
    return(data)
  }
  
  calculate_lutdaycount1_impute <- function(data, id_col, date_col, ovulation_col, menses_col) {
    id_col <- rlang::as_name(rlang::enquo(id_col))
    date_col <- rlang::as_name(rlang::enquo(date_col))
    ovulation_col <- rlang::as_name(rlang::enquo(ovulation_col))
    menses_col <- rlang::as_name(rlang::enquo(menses_col))
    
    data <- data %>%
      dplyr::arrange(.data[[id_col]], .data[[date_col]]) %>%
      dplyr::group_by(.data[[id_col]]) %>%
      dplyr::mutate(lutdaycount1_impute = NA_integer_) %>%
      dplyr::group_modify(~ {
        df <- .x
        ov_idx <- which(df[[ovulation_col]] == 1)
        
        for (i in ov_idx) {
          # Start from the ovulation day itself
          j <- i
          count <- 0
          
          while (j <= nrow(df) &&
                 (is.na(df[[menses_col]][j]) || df[[menses_col]][j] != 1)) {
            df$lutdaycount1_impute[j] <- count
            
            # Stop the day before menses == 1
            if (!is.na(df[[menses_col]][j + 1]) && df[[menses_col]][j + 1] == 1) {
              break
            }
            
            count <- count + 1
            j <- j + 1
          }
        }
        
        return(df)
      }) %>%
      dplyr::ungroup()
    
    return(data)
  }
  
  
  # Apply helper functions
  data <- calculate_lutdaycount_impute(
    data,
    id_col = !!id,
    date_col = !!date,
    ovulation_col = ovtoday_impute,
    menses_col =!!menses
  )
  
  data <- calculate_lutdaycount1_impute(
    data,
    id_col = !!id,
    date_col = !!date,
    ovulation_col = ovtoday_impute,
    menses_col =!!menses
  )
  
  
  # Calculate `lutmax_impute`
  # for (i in 1:(nrow(data) - 1)) {
  #   if (is.na(data$lutdaycount_impute[i + 1] &&
  #             !is.na(data$lutdaycount_impute[i]))) {
  #     data$lutmax_impute[(i - (data$lutdaycount_impute[i])):i] <- as.numeric(data$lutdaycount_impute[i])
  #   }
  # }
  data = data %>%
    dplyr::mutate(lutmax_impute = dplyr::case_when(!is.na(lutdaycount_impute) ~ 14, TRUE ~ NA))
  
  
  data <- data %>%
    dplyr::mutate(lutperc_impute = 
                    dplyr::if_else(
                      !is.na(lutmax_impute) & cycle_incomplete != 1 & mcyclength >= lower_cyclength_bound & mcyclength <= upper_cyclength_bound,
                      lutdaycount_impute / lutmax_impute,
                      NA
                    )
    )
  
  # lutperc is scaled from 0 to 1, so substracting 1 so that it is scaled from -1 to 0 for menses-centered scaled_cycleday 
  data$perclut_impute = data$lutperc_impute -1
  # 
  # # Calculating cyclic_lut_imp (luteal component using ovtoday_impute)
  # 
  data = data %>%
    dplyr::mutate(
      cyclic_lut1_imp = dplyr::case_when(
        !is.na(perclut_impute) ~ lutdaycount1_impute / (lutmax_impute + 1),
        TRUE ~ NA
      )
    )
  # 
  data$cyclic_lut_imp = -1*(1 - data$cyclic_lut1_imp)
  # 
  # # Calculate `lutdaycount_imp_ov`
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::mutate(
      lutdaycount_imp_ov = dplyr::lead(lutdaycount_impute),
      lutdaycount_imp_ov = dplyr::case_when(
        is.na(lutdaycount_imp_ov) | !!id != dplyr::lead(!!id) ~ NA_real_,
        TRUE ~ lutdaycount_imp_ov
      ), 
      lutmax_imp_ov =  dplyr::case_when(
        !is.na(lutdaycount_imp_ov) ~ 14,
        TRUE ~ NA
      ))
  # 
  # # Calculate `lutperc_imp_ov`
  data <- data %>%
    dplyr::mutate(
      lutperc_imp_ov = dplyr::case_when(
        !is.na(lutmax_imp_ov) &
          cycle_incomplete != 1 &
          mcyclength >= lower_cyclength_bound &
          mcyclength <= upper_cyclength_bound ~
          lutdaycount_imp_ov / lutmax_imp_ov,
        TRUE ~ NA_real_
      )
    )
  # 
  # # Calculating ovulation-centered cyclic_lut_imp_ov (luteal component using ovtoday_impute)
  data = data %>%
    dplyr::mutate(cyclic_lut_imp_ov = dplyr::case_when(!is.na(cyclic_lut_imp) ~ 1 + cyclic_lut_imp,
                                                       TRUE ~ cyclic_lut_ov))
  
  return(data)
}

process_follicular_phase_impute <- function(data, id, date, menses) {
  `%>%` <- magrittr::`%>%`
  
  # Dynamically handle input column names
  id <- rlang::enquo(id)
  date <- rlang::enquo(date)
  menses <- rlang::enquo(menses)
  
  # Group by ID and arrange by date
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::arrange(!!date, .by_group = TRUE) %>%
    dplyr::mutate(folmax_impute = NA, foldaycount_impute = NA)
  
  # Helper function to calculate `foldaycount_impute`
  calculate_foldaycount_impute <- function(data, id_col, menses_col) {
    foldaycount_impute <- NA
    last_id <- NULL
    
    for (i in 1:nrow(data)) {
      if (is.null(last_id) || last_id != data[[id_col]][i]) {
        # Restart counting when ID changes
        foldaycount_impute <- ifelse(data[[menses_col]][i] == 1, 0, NA)
      } else if (!is.na(foldaycount_impute)) {
        foldaycount_impute <- foldaycount_impute + 1
      }
      
      if (!is.na(foldaycount_impute) &&
          i >= 3 &&
          !is.na(data$ovtoday_impute[i]) &&
          !is.na(data$ovtoday_impute[i - 1]) &&
          data$ovtoday_impute[i - 1] == 1) {
        foldaycount_impute <- NA
      } else if (data[[menses_col]][i] == 1) {
        # Start counting when menses == 1
        foldaycount_impute <- 0
      }
      
      # Assign the foldaycount value to the current row
      data$foldaycount_impute[i] <- foldaycount_impute
      
      # Update last_id
      last_id <- data[[id_col]][i]
    }
    
    return(data)
  }
  
  # Apply helper function to calculate `foldaycount_impute`
  data <- calculate_foldaycount_impute(
    data,
    id_col = rlang::quo_name(id),
    menses_col = rlang::quo_name(menses)
  )
  
  # Create `folmax_impute` as the highest number `foldaycount_impute` counts up to
  # Same run-end logic as lutmax/folmax: a run closes when the next row is NA or
  # at the final dataset row. (The previous condition also mis-parenthesised the
  # `&&` inside `is.na()`; rewritten here to the explicit, correct form.)
  for (i in 1:nrow(data)) {
    if (!is.na(data$foldaycount_impute[i]) &&
        (i == nrow(data) || is.na(data$foldaycount_impute[i + 1]))) {
      data$folmax_impute[(i - (data$foldaycount_impute[i])):i] <- as.numeric(data$foldaycount_impute[i])
    }
  }
  
  # Create `percfol_impute`
  data <- data %>%
    dplyr::mutate(percfol_impute = ifelse(mcyclength >= lower_cyclength_bound & mcyclength <= upper_cyclength_bound & cycle_incomplete != 1, foldaycount_impute / folmax_impute, NA))
  
  # Create cyclic_fol_imp, follicular part of cyclic time, using ovtoday_impute 
  data$cyclic_fol_imp = data$percfol_impute
  
  # Create cyclic_fol_imp_ov, ovulation-centered follicular part of cyclic time, using ovtoday_impute 
  data = data %>% 
    dplyr::mutate(cyclic_fol_imp_ov = dplyr::case_when(!is.na(cyclic_fol_imp) ~ -1*(1 - cyclic_fol_imp), 
                                                       TRUE ~ cyclic_fol_ov))
  
  return(data)
}

create_scaled_cycleday <- function(data, id, date, menses) {
  `%>%` <- magrittr::`%>%`
  
  # Dynamically handle input column name
  id <- rlang::enquo(id)
  date <- rlang::enquo(date)
  menses <- rlang::enquo(menses)
  
  # Create `percentlut` and `percentfol`
  data <- data %>%
    dplyr::mutate(
      percentlut = dplyr::case_when(!is.na(percfol) ~ 0, TRUE ~ lutperc1),
      percentfol = dplyr::case_when(!is.na(lutperc1) ~ 0, TRUE ~ percfol)
    )
  
  # Create `scaled_cycleday`
  data <- data %>%
    dplyr::mutate(scaled_cycleday = ifelse(percentlut == 0, percentfol, percentlut))
  
  # Create ovulation-centered `scaled_cycleday_ov`
  data <- data %>%
    dplyr::mutate(scaled_cycleday_ov = ifelse(is.na(percfol_ov), lutperc_ov, percfol_ov))
  
  # Prioritize luteal phase measures created with ovtoday over ovtoday_impute
  data <- data %>%
    dplyr::mutate(
      group = cumsum(
        is.na(lutperc_imp_ov) &
          dplyr::lag(is.na(lutperc_imp_ov), default = TRUE) != is.na(lutperc_imp_ov)
      )
    ) %>%
    dplyr::group_by(!!id, group, cyclenum) %>%
    dplyr::mutate(perclut_ov_imp = if (any(!is.na(lutperc_ov) & lutperc_ov != -1)) lutperc_ov else lutperc_imp_ov) %>%
    dplyr::ungroup() %>%
    dplyr::select(-group)
  
  # Apply functions for imputation
  data <- data %>%
    dplyr::mutate(
      group = cumsum(
        is.na(perclut_impute) &
          dplyr::lag(is.na(perclut_impute), default = TRUE) != is.na(perclut_impute)
      )
    ) %>%
    dplyr::group_by(!!id, group, cyclenum) %>%
    dplyr::mutate(percentlut_impute = if (any(!is.na(lutperc1) & lutperc1 != 0)) lutperc1 else perclut_impute) %>% #besides 0 
    dplyr::ungroup() %>%
    dplyr::select(-group)
  
  data <- data %>%
    dplyr::mutate(
      group = cumsum(
        is.na(percfol_impute) &
          dplyr::lag(is.na(percfol_impute), default = TRUE) != is.na(percfol_impute)
      )
    ) %>%
    dplyr::group_by(!!id, group) %>%
    dplyr::mutate(percentfol_impute = if (any(!is.na(folperc))) folperc else percfol_impute) %>%
    dplyr::ungroup() %>%
    dplyr::select(-group)
  
  # Create ovulation-centered `percfol_ov_imp`
  data <- data %>%
    dplyr::mutate(
      percfol_ov_imp = ifelse(
        !is.na(percentfol_impute),
        percentfol_impute - 1,
        NA
      )
    )
  
  # Create `scaled_cycleday_impute` from `percentlut_impute` and `percentfol_impute`
  data <- data %>%
    dplyr::mutate(
      scaled_cycleday_impute = ifelse(
        is.na(percentfol_impute),
        percentlut_impute,
        percentfol_impute
      )
    )
  
  # Replace NA with 0 in `percentlut_impute` and `percentfol_impute`
  data$percentlut_impute <- ifelse(
    is.na(data$percentlut_impute) & !is.na(data$percentfol_impute),
    0,
    data$percentlut_impute
  )
  data$percentfol_impute <- ifelse(
    is.na(data$percentfol_impute) & !is.na(data$percentlut_impute),
    0,
    data$percentfol_impute
  )
  
  # Create ovulation-centered `scaled_cycleday_imp_ov` with imputed measures
  data <- data %>%
    dplyr::mutate(
      scaled_cycleday_imp_ov = ifelse(
        is.na(percfol_ov_imp),
        perclut_ov_imp,
        percfol_ov_imp
      )
    )
  
  # Create cyclic_time
  data = data %>% 
    dplyr::mutate(cyclic_time = dplyr::case_when(is.na(cyclic_lut) ~ cyclic_fol, 
                                                 TRUE ~ cyclic_lut))
  
  # Create cyclic_time_impute
  #
  # A confirmed ovulation whose luteal OR follicular phase individually exceeds
  # the package's internal cap (18d luteal / 25d follicular -- see
  # process_luteal_phase_base() / process_follicular_phase_base()) leaves
  # cyclic_time NA for that phase. The imputed-ovulation fallback
  # (cyclic_time_imp1) is ALSO NA in that case, because
  # calculate_ovtoday_impute() correctly suppresses imputed ovulation whenever
  # a confirmed ovulation already exists in the same cycle (to avoid
  # double-anchoring) -- which leaves no fallback at all for those days.
  #
  # cyclic_time_uncapped closes exactly that gap: the same phase math as
  # cyclic_lut / cyclic_fol, without the internal cap, gated on the cycle
  # still falling within the caller's own [lower_cyclength_bound,
  # upper_cyclength_bound] (see cyclic_lut_uncapped / folperc_uncapped). It is
  # used ONLY as the last-resort fallback here -- cyclic_time itself (the
  # confirmed-only, strict column) is computed above and never reads this.
  # cyclic_time_impute_extended_phase marks exactly which rows were filled
  # this way, so a downstream analyst can identify or exclude them.
  #
  # Excludes ovtoday==1 / ovtoday_impute==1 rows: calculate_cycletime()'s
  # final post-processing (below, after this function returns) unconditionally
  # overwrites cyclic_time_impute to 1 at the ovulation anchor itself,
  # regardless of which tier supplied a value here. Flagging those rows
  # "extended_phase" would be misleading -- their final value never actually
  # depends on the fallback, since the anchor override always wins there.
  data = data %>%
    dplyr::mutate(
      cyclic_time_imp1 = dplyr::case_when(is.na(cyclic_lut_imp) ~ cyclic_fol_imp, TRUE ~ cyclic_lut_imp),
      cyclic_time_uncapped = dplyr::coalesce(cyclic_lut_uncapped, cyclic_fol_uncapped),
      cyclic_time_impute_extended_phase = as.integer(
        is.na(cyclic_time) & is.na(cyclic_time_imp1) & !is.na(cyclic_time_uncapped) &
          !(!is.na(ovtoday) & ovtoday == 1) & !(!is.na(ovtoday_impute) & ovtoday_impute == 1)
      ),
      cyclic_time_impute = dplyr::coalesce(cyclic_time, cyclic_time_imp1, cyclic_time_uncapped)
    )
  
  # Create ovulation-centered cyclic_time_ov
  data = data %>% 
    dplyr::mutate(cyclic_time_ov = dplyr::case_when(is.na(cyclic_lut_ov) ~ cyclic_fol_ov, 
                                                    TRUE ~ cyclic_lut_ov))
  
  # Created ovulation-centered cyclic_time_imp_ov
  #
  # Same three-tier fallback as cyclic_time_impute above, mirrored for the
  # ovulation-centered variable: cyclic_time_ov (confirmed, strict) ->
  # cyclic_time_imp_ov1 (imputed-ovulation path) -> cyclic_time_ov_uncapped
  # (same phase math without the internal cap, gated on the caller's overall
  # cycle-length bound). cyclic_time_ov itself is never touched.
  # cyclic_time_imp_ov_extended_phase excludes the ovulation-anchor day for
  # the same reason cyclic_time_impute_extended_phase does: calculate_cycletime()'s
  # post-processing unconditionally pins cyclic_time_imp_ov to 0 at
  # ovtoday==1/ovtoday_impute==1, regardless of which tier supplied a value here.
  data = data %>%
    dplyr::mutate(
      cyclic_time_imp_ov1 = dplyr::case_when(is.na(cyclic_lut_imp_ov) ~ cyclic_fol_imp_ov,
                                             TRUE ~ cyclic_lut_imp_ov),
      cyclic_time_ov_uncapped = dplyr::coalesce(cyclic_lut_ov_uncapped, cyclic_fol_ov_uncapped),
      cyclic_time_imp_ov_extended_phase = as.integer(
        is.na(cyclic_time_ov) & is.na(cyclic_time_imp_ov1) & !is.na(cyclic_time_ov_uncapped) &
          !(!is.na(ovtoday) & ovtoday == 1) & !(!is.na(ovtoday_impute) & ovtoday_impute == 1)
      ),
      cyclic_time_imp_ov = dplyr::coalesce(cyclic_time_ov, cyclic_time_imp_ov1, cyclic_time_ov_uncapped)
    )
  
  # cyclic_time_impute / cyclic_time_imp_ov are about to be unconditionally
  # pinned at the menses anchor below, regardless of which tier supplied their
  # value above -- capture whether that override actually fires (same
  # predicate, read BEFORE the overwrite) so the two _extended_phase flags can
  # be corrected to match. Without this, a row whose published value is really
  # this menses-anchor pin can still be marked "filled by the uncapped
  # fallback," even though the pinned value has nothing to do with it (for
  # cyclic_time_imp_ov the pin, +1, actively disagrees with the fallback's own
  # -1 at that row; for cyclic_time_impute the pin, 0, currently happens to
  # coincide with the fallback's value there, masking the same underlying gap).
  data <- data %>%
    dplyr::arrange(!!date, .by_group = TRUE) %>%
    dplyr::group_by(!!id) %>%
    dplyr::mutate(
      menses_anchor_pins_impute = !!menses == 1 & !is.na(dplyr::lag(cyclic_time_impute)),
      menses_anchor_pins_imp_ov = !!menses == 1 & !is.na(dplyr::lag(cyclic_time_imp_ov)),
      scaled_cycleday_impute = dplyr::case_when(
        !!menses == 1 & !is.na(dplyr::lag(scaled_cycleday_impute)) ~ 0,
        TRUE ~ scaled_cycleday_impute
      ),
      cyclic_time_impute_extended_phase = dplyr::if_else(
        menses_anchor_pins_impute, 0L, cyclic_time_impute_extended_phase
      ),
      cyclic_time_impute = dplyr::case_when(
        !!menses == 1 & !is.na(dplyr::lag(cyclic_time_impute)) ~ 0,
        TRUE ~ cyclic_time_impute
      ),
      cyclic_time_imp_ov_extended_phase = dplyr::if_else(
        menses_anchor_pins_imp_ov, 0L, cyclic_time_imp_ov_extended_phase
      ),
      cyclic_time_imp_ov = dplyr::case_when(
        !!menses == 1 & !is.na(dplyr::lag(cyclic_time_imp_ov)) ~ 1,
        TRUE ~ cyclic_time_imp_ov
      ),
      scaled_cycleday_imp_ov = dplyr::case_when(
        !!menses == 1 & !is.na(dplyr::lag(scaled_cycleday_imp_ov)) ~ -1,
        TRUE ~ scaled_cycleday_imp_ov
      )
    ) %>%
    dplyr::select(-menses_anchor_pins_impute, -menses_anchor_pins_imp_ov)
  
  data <- data %>%
    dplyr::arrange(!!date) %>% # Ensure proper ordering
    dplyr::group_by(!!id) %>%
    dplyr::mutate(
      scaled_cycleday = dplyr::case_when(
        !!menses == 1 & !is.na(dplyr::lag(scaled_cycleday)) ~ 0,
        TRUE ~ scaled_cycleday
      ),
      cyclic_time = dplyr::case_when(
        !!menses == 1 & !is.na(dplyr::lag(cyclic_time)) ~ 0,
        TRUE ~ cyclic_time
      ),
      cyclic_time_ov = dplyr::case_when(
        !!menses == 1 & !is.na(dplyr::lag(cyclic_time_ov)) ~ 1,
        TRUE ~ cyclic_time_ov
      ),
      scaled_cycleday_ov = dplyr::case_when(
        !!menses == 1 & !is.na(dplyr::lag(scaled_cycleday_ov)) ~ -1,
        TRUE ~ scaled_cycleday_ov
      )
    )
  
  return(data)
}

