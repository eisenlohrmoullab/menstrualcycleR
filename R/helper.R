#' Process Data (Internal)
#'
#' Internal helper functions to process the data.
#'
#' @param data A data frame containing cycle data.
#' @param id A column specifying individual ids.
#' @param daterated A column specifying the dates.
#' @param menses A column indicating menses (0/1).
#'
#' @return A data frame with processed data.
#' @keywords internal
#' 
#' 

process_luteal_phase_base <- function(data, id, daterated, menses) {
  `%>%` <- magrittr::`%>%`
  
  # Quote column names for tidy evaluation
  id <- rlang::enquo(id)
  daterated <- rlang::enquo(daterated)
  menses <- rlang::enquo(menses)
  
  # Group and arrange data
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::arrange(!!daterated, .by_group = TRUE) %>%
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
  for (i in 1:(nrow(data) - 1)) {
    if (is.na(data$lutdaycount[i + 1]) && !is.na(data$lutdaycount[i])) {
      data$lutmax[(i - (data$lutdaycount[i])):i] <- as.numeric(data$lutdaycount[i])
    }
  }
  
  # Calculate lutperc and lutperc1
  data <- data %>%
    dplyr::mutate(
      lutperc = ifelse(lutmax <= 18 & lutmax >= 7, lutdaycount / lutmax, NA),
      lutperc1 = lutperc - 1
    )
  
  # Calculate lutdaycount_ov and lutperc_ov
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::mutate(
      lutdaycount_ov = dplyr::lead(lutdaycount),
      lutdaycount_ov = dplyr::case_when(
        is.na(lutdaycount_ov) | !!id != dplyr::lead(!!id) ~ NA,
        TRUE ~ lutdaycount_ov
      ),
      lutperc_ov = ifelse(lutmax <= 18 & lutmax >= 7, lutdaycount_ov / lutmax, NA),
      lutperc_ov = ifelse(lutdaycount_ov == 0, 0, lutperc_ov)
    )
  
  return(data)
}


process_follicular_phase_base <- function(data, id, daterated, menses) {
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
  daterated <- rlang::enquo(daterated)
  menses <- rlang::enquo(menses)
  
  # Processing logic remains unchanged
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::arrange(!!daterated, .by_group = TRUE) %>%
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
  for (i in 1:(nrow(data) - 1)) {
    if (is.na(data$foldaycount[i + 1]) && !is.na(data$foldaycount[i])) {
      data$folmax[(i - (data$foldaycount[i])):i] <- as.numeric(data$foldaycount[i])
    }
  }
  
  # Calculate follength = folmax + 1 (to include menses onset)
  data <- data %>%
    dplyr::mutate(follength = folmax + 1)
  
  # Calculate folperc (only when follicular length is between 8 and 25)
  # data <- data %>%
  #   dplyr::mutate(folperc = ifelse(follength >= 8 & follength <= 25, foldaycount / folmax, NA))
  data <- data %>%
    arrange(id, daterated) %>%  # Ensure data is sorted correctly
    mutate(
      next_id = lead(id),  # Get the id of the next row
      folperc = ifelse(
        follength >= 8 & follength <= 25 & lead(foldaycount == folmax) & id != next_id, 
        foldaycount / folmax, 
        NA
      )
    ) %>%
    select(-next_id)
  
  # Calculate percfol and percfol_ov
  data <- data %>% 
    dplyr::mutate(
      percfol = dplyr::case_when(
        !is.na(lutperc1) & !is.na(folperc) & folperc != 0 ~ NA,
        TRUE ~ folperc
      ),
      percfol_ov = percfol - 1
    )
  return(data)
}



calculate_ovtoday_impute <- function(data, id, daterated, menses) {
  `%>%` <- magrittr::`%>%`
  
  # Dynamically handle input column names
  id <- rlang::enquo(id)
  daterated <- rlang::enquo(daterated)
  menses <- rlang::enquo(menses)
  
  # Step 1: Calculate `lutlength_impute` and `follength_impute`
  data <- data %>%
    dplyr::mutate(
      lutlength_impute = dplyr::case_when(
        mcyclength == 21 ~ (mcyclength * 0.433), #9.1/21
        mcyclength == 22 ~ (mcyclength * 0.445), #9.8/22
        mcyclength == 23 ~ (mcyclength * 0.448), #10.3/23
        mcyclength == 24 ~ (mcyclength * 0.45), #10.8/24
        mcyclength == 25 ~ (mcyclength * 0.444), #11.1/25
        mcyclength == 26 ~ (mcyclength * 0.442), #11.5/26
        mcyclength == 27 ~ (mcyclength * 0.433), #11.7/27
        mcyclength == 28 ~ (mcyclength * 0.425), #11.9/28
        mcyclength == 29 ~ (mcyclength * 0.414), #12/29
        mcyclength == 30 ~ (mcyclength * 0.403), #12.1/30
        mcyclength == 31 ~ (mcyclength * 0.394), #12.2/31
        mcyclength == 32 ~ (mcyclength * 0.384), #12.3/32
        mcyclength == 33 ~ (mcyclength * 0.370), #12.2/33
        mcyclength == 34 ~ (mcyclength * 0.362), #12.3/34
        mcyclength == 35 ~ (mcyclength * 0.349), #12.2/35
        TRUE ~ NA
      ),
      follength_impute = dplyr::case_when(
        mcyclength == 21 ~ (mcyclength * 0.567), #11.9/21
        mcyclength == 22 ~ (mcyclength * 0.554), #12.2/22 
        mcyclength == 23 ~ (mcyclength * 0.552), #12.7/23
        mcyclength == 24 ~ (mcyclength * 0.550), #13.2/24
        mcyclength == 25 ~ (mcyclength * 0.556), #13.9/25
        mcyclength == 26 ~ (mcyclength * 0.558), #14.5/26
        mcyclength == 27 ~ (mcyclength * 0.567), #15.3/27
        mcyclength == 28 ~ (mcyclength * 0.575), #16.1/28
        mcyclength == 29 ~ (mcyclength * 0.586), #17/29
        mcyclength == 30 ~ (mcyclength * 0.597), #17.9/30
        mcyclength == 31 ~ (mcyclength * 0.606), #18.8/31
        mcyclength == 32 ~ (mcyclength * 0.616), #19.7/32
        mcyclength == 33 ~ (mcyclength * 0.630), #20.8/33
        mcyclength == 34 ~ (mcyclength * 0.638), #21.7/34
        mcyclength == 35 ~ (mcyclength * 0.651), #22.8/35
        TRUE ~ NA
      )
    )
  
  # Step 2: Group by `id` and sort by `daterated`
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::arrange(!!daterated, .by_group = TRUE)
  
  # Step 3: Calculate `follcount1_impute`
  data$follcount1_impute <- NA
  for (i in 1:nrow(data)) {
    if (data[[rlang::as_name(menses)]][i] == 1 & !is.na(data$follength_impute[i])) {
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
        round(follength_impute) == follcount1_impute ~ 1,
        TRUE ~ NA
      )
    )
  
  # Step 5: Replace NA values in `ovtoday_impute` with 0
  data$ovtoday_impute <- ifelse(is.na(data$ovtoday_impute), 0, data$ovtoday_impute)
  
  
  # Step 6: If ovtoday and ovtoday_impute are within 5 days of each other, ovtoday_impute == 0
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::arrange(!!daterated, .by_group = TRUE) %>%
    dplyr::mutate(
      ovtoday_impute = ifelse(
        ovtoday_impute == 1 & sapply(!!daterated, function(date) {
          any(abs(difftime(daterated[ovtoday == 1], date, units = "days")) <= 7)
        }),
        0,
        ovtoday_impute
      )
    ) %>%
    dplyr::ungroup()
  
  
  return(data)
}


process_luteal_phase_impute <- function(data, id, daterated, menses) {
  `%>%` <- magrittr::`%>%`
  
  # Dynamically handle input column names
  id <- rlang::enquo(id)
  daterated <- rlang::enquo(daterated)
  menses <- rlang::enquo(menses)
  
  # Group by ID and arrange by date
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::arrange(!!daterated, .by_group = TRUE) %>%
    dplyr::mutate(lutmax_impute = NA)
  
  # Helper function to calculate `lutdaycount1_impute`
  calculate_lutdaycount_impute <- function(data, id_col, menses_col) {
    last_id <- NULL
    lutdaycount1_impute <- rep(NA, nrow(data))
    
    for (i in 1:nrow(data)) {
      if (is.null(last_id) || last_id != data[[id_col]][i]) {
        lutdaycount1_impute[i] <- ifelse(data$ovtoday_impute[i] == 1, 0, NA)
      } else if (!is.na(lutdaycount1_impute[i - 1])) {
        lutdaycount1_impute[i] <- lutdaycount1_impute[i - 1] + 1
      }
      
      if (!is.na(lutdaycount1_impute[i]) &&
          !is.na(data[[menses_col]][i]) &&
          data[[menses_col]][i] == 1) {
        lutdaycount1_impute[i] <- NA
      } else if (data$ovtoday_impute[i] == 1) {
        lutdaycount1_impute[i] <- 0
      }
      
      last_id <- data[[id_col]][i]
    }
    
    data$lutdaycount1_impute <- lutdaycount1_impute
    return(data)
  }
  
  # Apply helper function
  data <- calculate_lutdaycount_impute(
    data,
    id_col = rlang::quo_name(id),
    menses_col = rlang::quo_name(menses)
  )
  
  # Calculate `lutdaycount_impute`
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::mutate(
      lutdaycount_impute = dplyr::lag(lutdaycount1_impute),
      lutdaycount_impute = dplyr::case_when(
        is.na(lutdaycount_impute) | !!id != dplyr::lag(!!id) ~ NA,
        TRUE ~ lutdaycount_impute
      )
    )
  
  # Calculate `lutmax_impute`
  for (i in 1:(nrow(data) - 1)) {
    if (is.na(data$lutdaycount_impute[i + 1] &&
              !is.na(data$lutdaycount_impute[i]))) {
      data$lutmax_impute[(i - (data$lutdaycount_impute[i])):i] <- as.numeric(data$lutdaycount_impute[i])
    }
  }
  
  # Lag `lutlength_impute` to align with `lutdaycount`
  data$lutlength1_impute <- dplyr::lag(data$lutlength_impute)
  
  data <- data %>%
    dplyr::mutate(lutperc_impute = dplyr::if_else(
      is.na(lutlength1_impute),
      NA,
      dplyr::if_else(
        is.na(lutlength1_impute) &
          !is.na(lutmax_impute) & cycle_incomplete == 0 & mcyclength > 20 & mcyclength < 36,
        lutdaycount_impute / lutmax_impute,
        lutdaycount_impute / round(lutlength1_impute)
      )
    ))
  
  #lutperc is scaled from 0 to 1, so substracting 1 so that it is scaled from -1 to 0 for menses-centered scaled_cycleday 
  data$perclut_impute = data$lutperc_impute -1 
  
  # Calculate `lutdaycount_imp_ov`
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::mutate(
      lutdaycount_imp_ov = dplyr::lead(lutdaycount_impute),
      lutdaycount_imp_ov = dplyr::case_when(
        is.na(lutdaycount_imp_ov) | !!id != dplyr::lead(!!id) ~ NA_real_,
        TRUE ~ lutdaycount_imp_ov
      )
    )
  
  # Calculate `lutperc_imp_ov`
  data <- data %>%
    dplyr::mutate(lutperc_imp_ov = dplyr::if_else(
      is.na(lutlength1_impute) ,
      NA,
      dplyr::if_else(
        is.na(lutlength1_impute) & !is.na(lutmax) & cycle_incomplete == 0 & mcyclength > 20 & mcyclength < 36,
        lutdaycount_imp_ov / lutmax_impute,
        lutdaycount_imp_ov / round(lutlength1_impute)
      )
    ))
  

    #  %>%
    # dplyr::mutate(
    #   lutperc_imp_ov = ifelse(
    #     ovtoday_impute == 1,
    #     0,
    #     lutperc_imp_ov
    #   )
    # )

  
  return(data)
}


process_follicular_phase_impute <- function(data, id, daterated, menses) {
  `%>%` <- magrittr::`%>%`
  
  # Dynamically handle input column names
  id <- rlang::enquo(id)
  daterated <- rlang::enquo(daterated)
  menses <- rlang::enquo(menses)
  
  # Group by ID and arrange by date
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::arrange(!!daterated, .by_group = TRUE) %>%
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
          data$ovtoday_impute[i - 1] == 1) {
        # Stop counting one row after ovtoday == 1
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
  for (i in 1:(nrow(data) - 1)) {
    if (is.na(data$foldaycount_impute[i + 1] &&
              !is.na(data$foldaycount_impute[i]))) {
      data$folmax_impute[(i - (data$foldaycount_impute[i])):i] <- as.numeric(data$foldaycount_impute[i])
    }
  }
  
  # Create `percfol_impute`, based on Bull 2019 norms
  data <- data %>%
    dplyr::mutate(percfol_impute = ifelse(mcyclength >= 21 & mcyclength <= 35, foldaycount_impute / folmax_impute, NA))
  
  return(data)
}


create_scaled_cycleday <- function(id, data) {
  `%>%` <- magrittr::`%>%`
  
  # Dynamically handle input column name
  id <- rlang::enquo(id)
  
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
  
  return(data)
}


