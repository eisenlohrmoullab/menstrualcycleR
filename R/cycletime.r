#' Create Cycle Time Variables
#'
#' This function calculates and adds cycle time variables to a longitudinal dataset. 
#' It should be applied **after** the `mcyclength()` function has been used on the dataset.
#' 
#' The dataset must be in long format, where each row represents a unique date for a specific individual.
#' 
#' @keywords menstrual cycle, cycle time, ovulation
#'
#' @param data A data frame containing the required input variables.
#' @param id A unique identifier for individuals in the dataset.
#' @param daterated A date column indicating when the data was recorded.
#' @param menses A binary column (`0`/`1`) indicating the first day of menses onset, where `1` represents menses onset.
#' @param ovtoday A binary column (`0`/`1`) indicating the estimated day of ovulation, where `1` represents ovulation.
#'
#' @return The input data frame with the following additional variables:
#' 
#' - **`scaled_cycleday`**: Centered on menses (`menses == 1` corresponds to `scaled_cycleday == 0`) and spans from `-1` (the first day of the luteal phase) to `+1` (the estimated day of ovulation).
#' 
#' - **`scaled_cycleday_impute`**: Similar to `scaled_cycleday`, but includes scaling based on imputed ovulation (`ovtoday_impute`) for cycles without confirmed ovulation (`ovtoday`). This variable spans from `-1` (the first day of the luteal phase) to `+1` (the estimated day of ovulation, either `ovtoday` or `ovtoday_impute`).
#' 
#' - **`scaled_cycleday_ov`**: Centered on confirmed ovulation (`ovtoday == 1` corresponds to `scaled_cycleday_ov == 0`) and spans from `-1` (the first day of the follicular phase) to `+1` (the last day of the luteal phase).
#' 
#' - **`scaled_cycleday_imp_ov`**: Similar to `scaled_cycleday_ov`, but includes scaling based on imputed ovulation (`ovtoday_impute`) for cycles without confirmed ovulation. This variable is centered on ovulation (`ovtoday == 1` or `ovtoday_impute == 1` corresponds to `scaled_cycleday == 0`) and spans from `-1` to `+1`.
#'
#' @export
#'
#' @examples 
#' # Load the example dataset
#' data(cycledata)
#'
#' # First, calculate menses-to-menses cycle lengths
#' data <- calculate_mcyclength(
#'   data, 
#'   id = id, 
#'   daterated = daterated, 
#'   menses = menses, 
#'   ovtoday = ovtoday
#' )
#'
#' # Then, create cycle time variables
#' data <- calculate_cycletime(
#'   data, 
#'   id = id, 
#'   daterated = daterated, 
#'   menses = menses, 
#'   ovtoday = ovtoday
#' )
#'
#' # View the result
#' print(data)



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
  
  data <- data %>% 
    dplyr::mutate(menses = !!menses)
  
  data <- data %>% 
    dplyr::mutate(daterated = !!daterated)
  
  data <- data %>% 
    dplyr::mutate(id = !!id)
  
  # Group and arrange data by ID and date
  data <- data %>%
    dplyr::group_by(!!id) %>%
    dplyr::arrange(!!daterated, .by_group = TRUE)
  
  # Apply the processing functions in sequence
  data <- process_luteal_phase_base(data, id, daterated, menses)
  data <- process_follicular_phase_base(data, id, daterated, menses)
  data <- calculate_ovtoday_impute(data, id, daterated, menses)
  data <- process_luteal_phase_impute(data, id, daterated, menses)
  data <- process_follicular_phase_impute(data, id, daterated, menses)
  data <- create_scaled_cycleday(id, data)
  
  Select and return the relevant columns
  data <- data %>%
    dplyr::select(
      c(
        dplyr::all_of(varnames),
        ovtoday_impute,
        scaled_cycleday,
        scaled_cycleday_ov,
        scaled_cycleday_impute,
        scaled_cycleday_imp_ov
      )
    )

  return(data)
}


