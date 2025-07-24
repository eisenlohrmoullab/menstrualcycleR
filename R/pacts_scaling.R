#' Create Cycle Time Variables
#'
#' This function calculates and adds cycle time variables to a longitudinal dataset. 
#' 
#' The dataset must be in long format, where each row represents a unique date for a specific individual.
#' 
#' @keywords menstrual cycle, cycle time, ovulation
#'
#' @param data A data frame containing the required input variables.
#' @param id A unique identifier for individuals in the dataset.
#' @param date A date column indicating when the data was recorded.
#' @param menses A binary column (`0`/`1`) indicating the first day of menses onset, where `1` represents menses onset.
#' @param ovtoday A binary column (`0`/`1`) indicating the estimated day of ovulation, where `1` represents ovulation.
#' @param lower_cyclength_bound A numeric that indicates the lower bound of cycle lengths that the function will scale, the default is 21 
#' @param upper_cyclength_bound A numeric that indicates the upper bound of cycle lengths that the function will scale, the default is 35
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
#' - **`ovtoday_impute`**: If ovulation is not indicated in the column inputted for the parameter `ovtoday` in a menses-to-menses cycle, `ovtoday_impute` will be calculated, which estimates ovulation as day -15 of the cycle. 
#'
#' @export
#'
#' @examples 
#' # Load the example dataset
#' data(cycledata)
#'
#' data_with_scaling <- pacts_scaling(
#'   data, 
#'   id = id, 
#'   daterated = daterated, 
#'   menses = menses, 
#'   ovtoday = ovtoday, 
#'   lower_cyclength_bound = 21, 
#'   upper_cyclength_bound = 35
#' )
#'
#'
#' # View the result
#' print(data_with_scaling)
#' 

pacts_scaling <- function(data, id, date, menses, ovtoday, lower_cyclength_bound = 21, upper_cyclength_bound = 35) {
  data = calculate_mcyclength(data, id, date, menses, ovtoday)
  data = calculate_cycletime(data, id, date, menses, ovtoday, lower_cyclength_bound, upper_cyclength_bound)
  return(data)
}
