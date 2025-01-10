#' Calculate Menses-to-Menses Cycle Lengths
#'
#' This function calculates the lengths of menses-to-menses cycles for individuals in a longitudinal dataset. 
#' The input dataset must be in long format, where each row represents a unique date for a specific individual.
#' 
#' - The `menses` column should indicate the first day of menses onset for each individual, coded as `1` (menses) or `0` (no menses).
#' - The `daterated` column should be a date variable recording when the data was collected.
#' - The `ovtoday` column should indicate estimated ovulation days, also coded as `1` (ovulation day) or `0` (non-ovulation day).
#'
#' @keywords menstrual cycle, menses, cycle analysis
#'
#' @param data A data frame containing the required input variables.
#' @param id A unique identifier for individuals in the dataset.
#' @param daterated A date column indicating when the data was recorded.
#' @param menses A binary column (`0`/`1`) indicating the first day of menses onset, where `1` represents menses onset.
#' @param ovtoday A binary column (`0`/`1`) indicating the estimated day of ovulation, where `1` represents ovulation day.
#'
#' @return The input data frame with the additional following variables:
#' - **`mcyclength`**: The length of a consecutive menses-to-menses cycle, measured in days.
#' - **`m2mcount`**: A forward count starting from each menses onset.
#' - **`cyclenum`**: The number of complete menses-to-menses cycles for each individual (`id`).
#' - **`cycle_incomplete`**: A binary variable (`0`/`1`), where `1` indicates that the row is not part of a complete cycle.
#'
#' @export
#' @examples 
#' # Load the example dataset
#' data(cycledata)
#' 
#' # Calculate menses-to-menses cycle lengths
#' result <- calculate_mcyclength(data = data, 
#'                                 id = id, 
#'                                 daterated = daterated, 
#'                                 menses = menses, 
#'                                 ovtoday = ovtoday)
#' 
#' # View the result
#' print(result)


calculate_mcyclength <- function(data, id, daterated, menses, ovtoday) {
  `%>%` <- magrittr::`%>%`
  `:=` <- rlang::`:=`
  
  # Ensure variable and daterated are evaluated correctly
  menses <- rlang::enquo(menses)
  daterated <- rlang::enquo(daterated)
  id <- rlang::enquo(id)
  ovtoday <- rlang::enquo(ovtoday)
  
  # Ensure daterated is in Date format
  data <- data %>%
    dplyr::mutate(!!rlang::quo_name(daterated) := as.Date(!!daterated))
  
  # Group and complete daterated sequence
  data <- data %>%
    dplyr::group_by(!!id) %>%
    tidyr::complete(!!daterated := seq.Date(min(!!daterated), max(!!daterated), by = "day"))
  
  #Create ovtoday 
  # data <- data %>% dplyr::arrange(id, daterated) %>%
  #   dplyr::group_by(id) %>%
  #   dplyr::mutate(ovtoday = dplyr::lag(LHtest))
  
  #Turn NAs to 0 for menses, ovtoday, and LHtest 
 
  data <- data %>%
    dplyr::mutate(
      !!rlang::quo_name(menses) := ifelse(is.na(!!menses), 0, !!menses),
      !!rlang::quo_name(ovtoday) := ifelse(is.na(!!ovtoday), 0, !!ovtoday)
    )
  

  
  # data$LHtest <- ifelse(is.na(data$LHtest), 0, data$LHtest) 
  
  # Initialize columns
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::arrange(!!daterated, .by_group = TRUE) %>%
    dplyr::mutate(
      m2mcount = NA_integer_,
      mcyclength = NA_integer_,
      cycle_incomplete = 0
    )
  
  # Loop to calculate m2mcount
  for (i in seq_len(nrow(data))) {
    if (!is.na(dplyr::pull(data, !!menses)[i]) && dplyr::pull(data, !!menses)[i] == 1) {
      data$m2mcount[i] <- 1
      j <- i + 1
      
      while (j <= nrow(data) && data$id[j] == data$id[i] && (is.na(dplyr::pull(data, !!menses)[j]) || dplyr::pull(data, !!menses)[j] != 1)) {
        data$m2mcount[j] <- data$m2mcount[j - 1] + 1
        j <- j + 1
      }
      
      if (j <= nrow(data) && data$id[j] == data$id[i] && !is.na(dplyr::pull(data, !!menses)[j]) && dplyr::pull(data, !!menses)[j] == 1) {
        data$m2mcount[j] <- data$m2mcount[j - 1] + 1
      }
    }
  }
  
  # Identify incomplete cycles
  data <- data %>%
    dplyr::mutate(cycle_incomplete = ifelse(!is.na(m2mcount) & 
                                       (is.na(dplyr::lead(m2mcount)) & id != dplyr::lead(!!id)), 1, 0))
  
  # Set cycle_incomplete = 1 if m2mcount restarts when id changes
  data <- data %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(cycle_incomplete = ifelse(
      !!id != dplyr::lag(!!id, default = dplyr::first(id)) & m2mcount == 1, 1, cycle_incomplete
    )) %>%
    dplyr::ungroup()
  
  # Propagate cycle_incomplete within each group of m2mcount and calculate mcyclength
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::mutate(cycle_group = cumsum(!is.na(m2mcount) & m2mcount == 1)) %>%
    dplyr::group_by(!!id, cycle_group) %>%
    dplyr::mutate(
      cycle_incomplete = dplyr::if_else(any(cycle_incomplete == 1), 1, 0),
      mcyclength = dplyr::case_when(
        all(is.na(m2mcount)) ~ NA_real_,                      
        all(!is.finite(m2mcount)) ~ NA_real_,                 
        TRUE ~ max(m2mcount, na.rm = TRUE)                    # Compute max if valid values exist
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-cycle_group)
  
  data$cycle_incomplete <- ifelse(is.na(data$cycle_incomplete), 1, data$cycle_incomplete)
  data$cycle_incomplete <- ifelse(is.na(data$m2mcount), NA, data$cycle_incomplete)
  
  # Calculate cyclenum: number of menses-to-menses cycles within a person
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::mutate(cyclenum = cumsum(!is.na(m2mcount) & m2mcount == 1 & cycle_incomplete == 0)) %>%
    dplyr::ungroup()
  
  # Handle cyclenum for incomplete cycles
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::mutate(cyclenum = ifelse(
      cycle_incomplete == 1,
      NA,
      cumsum(!is.na(m2mcount) & m2mcount == 1 & cycle_incomplete == 0)
    )) %>%
    dplyr::ungroup()
  #If mcyclength = -inf, turn to NA 
  data <- data %>%
    dplyr::mutate(
      mcyclength = dplyr::case_when(
        mcyclength == -Inf ~ NA,
        TRUE ~ mcyclength
      )
    )
  

  return(data)
}
