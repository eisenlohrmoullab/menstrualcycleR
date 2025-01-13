
library(testthat)


# Sample dataset for testing
sample_data <- data.frame(
  id = c(1, 1, 1, 2, 2, 2),
  menses = c(1, 0, 0, 1, 0, NA),
  ovtoday = c(NA, NA, 1, NA, 1, NA),
  stringsAsFactors = FALSE
)


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
  

  return(data)
}





calculate_cycletime <- function(data, id, daterated, menses, ovtoday) {
  `%>%` <- magrittr::`%>%`
  # Check if input data is a data frame
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  
  # Store original variable names
  varnames <- names(data)
  
  # Quote the column names for tidy evaluation
  id <- rlang::enquo(id)
  daterated <- rlang::enquo(daterated)
  menses <- rlang::enquo(menses)
  ovtoday <- rlang::enquo(ovtoday)
  
  # Create ovtoday 
  data <- data %>% 
    dplyr::mutate(ovtoday = !!ovtoday)
  
  # Group and arrange data by ID and date
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::arrange(!!daterated, .by_group = TRUE)
  
  # Apply the processing functions in sequence
  #data <- process_luteal_phase_base(data, id, daterated, menses)
  data <- process_follicular_phase_base(data, id, daterated, menses)
  #data <- calculate_ovtoday_impute(data, id, daterated, menses)
  #data <- process_luteal_phase_impute(data, id, daterated, menses)
  #data <- process_follicular_phase_impute(data, id, daterated, menses)
  #data <- create_scaled_cycleday(id, data)
  
  # Select and return the relevant columns
  # data <- data %>%
  #   dplyr::select(
  #     c(
  #       dplyr::all_of(varnames),
  #       ovtoday_impute,
  #       scaled_cycleday,
  #       scaled_cycleday_ov,
  #       scaled_cycleday_impute,
  #       scaled_cycleday_imp_ov
  #     )
  #   )
  
  return(data)
}




test_that("Input validation works", {
  # Test empty data
  empty_data <- data.frame()
  expect_error(
    process_follicular_phase_base(empty_data, id, daterated = NULL, menses),
    "Input data must include the following missing columns: id, menses, ovtoday."
  )
  
  # Test missing columns
  incomplete_data <- data.frame(id = c(1, 2))
  expect_error(
    process_follicular_phase_base(incomplete_data, id, daterated = NULL, menses),
    "Input data must include the following missing columns: menses, ovtoday."
  )
})



test_that("calculate_cycletime works with valid data", {
  test_data <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    daterated = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-02-01", "2023-02-02", "2023-02-03")),
    menses = c(1, 0, 0, 1, 0, 0),
    ovtoday = c(NA, NA, 1, NA, 1, NA)
  )
  
  result <- calculate_cycletime(test_data, id, daterated, menses, ovtoday)
  
  # Verify foldaycount is calculated
  expect_equal(result$foldaycount, c(0, 1, 2, 0, 1, 2))
})

test_that("calculate_cycletime handles missing columns", {
  test_data <- data.frame(id = c(1, 2), daterated = as.Date(c("2023-01-01", "2023-01-02")))
  
  expect_error(
    calculate_cycletime(test_data, id, daterated, menses, ovtoday),
    "Input data must include the following missing columns: menses, ovtoday."
  )
})






test_that("Edge cases in process_follicular_phase_base", {
  # Test with no menses data
  no_menses_data <- data.frame(
    id = c(1, 1, 1),
    menses = c(0, 0, 0),
    ovtoday = c(NA, NA, 1)
  )
  result <- process_follicular_phase_base(no_menses_data, id, daterated = NULL, menses)
  expect_true(all(is.na(result$foldaycount)))
  
  # Test with single-row data
  single_row_data <- data.frame(
    id = c(1),
    menses = c(1),
    ovtoday = c(NA)
  )
  result <- process_follicular_phase_base(single_row_data, id, daterated = NULL, menses)
  expect_equal(result$foldaycount, c(0))
})




