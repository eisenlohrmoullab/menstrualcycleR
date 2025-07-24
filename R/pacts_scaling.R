#' Adds Cycle Time Variables to Longitudinal Menstrual Cycle Data
#'
#' This function computes and adds menstrual cycle time variables to a long-format dataset, where each row represents a unique date for a specific individual.
#'
#' The dataset must include a column indicating the estimated day of ovulation. Ovulation can be assessed using several common methods in menstrual cycle research:
#' - **Luteinizing hormone (LH) urinary tests**: Ovulation is estimated as the day after a positive LH surge (i.e., LH +1), typically using a threshold of 40 mIU/ml.
#' - **Basal body temperature (BBT)**: Ovulation is estimated as the day after the lowest point in BBT prior to the rise (i.e., BBT +1).
#'
#' Before using this function, ovulation must be coded accordingly, with a value of `1` on the estimated day of ovulation in the `ovtoday` column and `0` or `NA` elsewhere.
#'
#' If ovulation was not assessed in your study, you must still include a placeholder `ovtoday` column (filled with `0` or `NA`). For cycles without a marked ovulation day (`ovtoday == 1`), the function will impute the ovulation day by counting backward 15 days from the onset of the next menses, based on the relatively stable length of the luteal phase. These imputed days will be marked with `1` in a new column named `ovtoday_impute` in the output.
#'
#' For additional guidance on assessing ovulation or using backward-count methods to estimate ovulation, see:
#'
#' - Schmalenberger, K. M., Tauseef, H. A., Barone, J. C., Owens, S. A., Lieberman, L., Jarczok, M. N., et al. (2021). How to study the menstrual cycle: Practical tools and recommendations. *Psychoneuroendocrinology, 123*, 104895. https://doi.org/10.1016/j.psyneuen.2020.104895  
#'
#' - Nagpal, A., Schmalenberger, K. M., Barone, J., Mulligan, E. M., Stumper, A., Knol, L., et al. (2025). Studying the Menstrual Cycle as a Continuous Variable: Implementing Phase-Aligned Cycle Time Scaling (PACTS) with the `menstrualcycleR` package. https://doi.org/10.31219/osf.io/hd5xw_v1
#' 
#' @keywords menstrual cycle, cycle time, ovulation
#'
#' @param data A data frame containing the required input variables.
#' @param id A unique identifier for individuals in the dataset.
#' @param date A date column indicating the day in which outcomes were reported on/observed.Keep in mind that prior to using this function, you may want to adjust dates for late-night entries (e.g. reclassify post-midnight surveys to the previous day).
#' @param menses A binary column (`0`/`1`) indicating the first day of menses onset, where `1` represents menses onset.
#' @param ovtoday A binary column (`0` or `1`) indicating the estimated day of ovulation, where `1` marks the day of ovulation. This column is required, even if ovulation was not directly assessed—if unavailable, include a column of `NA` or `0`s. 
#' Ovulation can be estimated using methods such as:
#' - **Luteinizing hormone (LH) tests**: Ovulation is typically estimated as the day after a positive LH test (LH +1), using a threshold of 40 mIU/ml for urinary LH.
#' - **Basal body temperature (BBT)**: Ovulation is estimated as the day after the BBT nadir (BBT +1).
#' 
#' This function requires the `ovtoday` column to be coded accordingly (e.g., `1` on LH +1 or BBT +1). 

#' If ovulation was not assessed for a given menses-to-menses cycle (i.e., no `1` in the `ovtoday` column), the function will estimate ovulation as 15 days before the next menses onset, based on the typical luteal phase length. These imputed ovulation days will be indicated in a new binary column, `ovtoday_impute`, in the output.

#' For more information regarding the validity of estimating ovulation via day -15 in the absence of biomarkers, see: Schmalenberger et al. (2021) and Nagpal et al. (2025).

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
#' - **`ovtoday_impute`**: If ovulation is not indicated in the column inputted for the parameter `ovtoday` in a menses-to-menses cycle, `ovtoday_impute` will be calculated, which estimates ovulation as day -15 of the cycle. For information regarding the validity of day -15 as estimated day of ovulation see: Schmalenberger et al. (2021) and Nagpal et al. (2025).
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
#'   date = date, 
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
  id <- rlang::enquo(id)
  date <- rlang::enquo(date)
  menses <- rlang::enquo(menses)
  ovtoday <- rlang::enquo(ovtoday)
  
  data = data %>%
   dplyr:: mutate(
      id = !!id,
      date = !!date,
      menses = !!menses,
      ovtoday = !!ovtoday
    )
  
  data = calculate_mcyclength(data, id, date, menses, ovtoday)
  data = calculate_cycletime(data, id, date, menses, ovtoday, lower_cyclength_bound, upper_cyclength_bound)
  return(data)
}
