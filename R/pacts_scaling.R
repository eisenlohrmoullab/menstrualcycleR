#' Phase-Aligned Cycle Time Scaling (PACTS)
#'
#' This function uses the Phase-Aligned Cycle Time Scaling (PACTS) method, as described in Nagpal et al. (2025), to compute continuous, biologically meaningful cycle time variables in a long-format dataset, where each row represents a unique observation date for a specific naturally-cycling individual.
#'
#' The PACTS method aligns observations across cycles by centering time either on menses onset or ovulation. This allows researchers to model menstrual cycle dynamics as continuous functions of time, improving sensitivity and interpretability. The function requires identification of menses onset (`menses`) and the estimated day of ovulation (`ovtoday`), which may be determined via biomarker (preferred) or imputed based on typical luteal phase length when unavailable.
#'
#' When ovulation is not directly assessed, the function imputes ovulation as 15 days prior to the next menses onset (i.e., the last day of the follicular phase), based on the population-average luteal phase length. Imputed ovulation days are recorded in a new binary column, `ovtoday_impute`.
#'
#' Reporting how often ovulation was confirmed using biomarkers versus imputed is important for transparency and scientific rigor. Whenever possible, researchers should use objective biomarkers such as LH tests or basal body temperature (BBT) to identify ovulation, as these methods provide greater precision. This function supports both confirmed and imputed ovulation, allowing analyses to flexibly account for variable data availability across participants and cycles.
#'
#' For further guidance on ovulation identification and justification of the -15 day imputation approach, see:
#' - Nagpal et al. (2025). *Studying the Menstrual Cycle as a Continuous Variable: Implementing Phase-Aligned Cycle Time Scaling (PACTS) with the `menstrualcycleR` package*. https://doi.org/10.31219/osf.io/hd5xw_v1  
#' - Schmalenberger et al. (2021). *How to study the menstrual cycle: Practical tools and recommendations*. *Psychoneuroendocrinology, 123*, 104895. https://doi.org/10.1016/j.psyneuen.2020.104895
#'
#' @param data A data frame containing the required input variables.
#' @param id A unique identifier for each naturally-cycling individual in the dataset.
#' @param date A date column corresponding to the day of observation. Ensure all id-date combinations are unique. You may wish to reclassify post-midnight survey entries to the previous day to maintain alignment with sleep or daily tracking data.
#' @param menses A binary column (`0`/`1`) indicating the *first* day of menses onset, where `1` marks onset. All subsequent bleeding days should be set to `0`. Periovulatory spotting should also be excluded.
#' @param ovtoday A binary column (`0`/`1`) indicating the day of estimated ovulation. Required even if biomarkers were not collected—use a column of all `0`s or `NA`s in that case. Accepted ovulation determination methods include:
#' \itemize{
#'   \item \strong{Urinary LH surge tests}: Code `ovtoday == 1` on the day *after* the first positive test (LH +1). Specify the brand and threshold (e.g., 40 mIU/ml).
#'   \item \strong{Basal body temperature (BBT)}: Code `ovtoday == 1` on the day after the BBT nadir (BBT +1). Specify measurement method.
#'   \item \strong{Daily hormone assays}: See Nagpal et al. (2025) for guidance.
#' }
#' @param lower_cyclength_bound Numeric lower bound of cycle lengths to include in scaling. Default is 21.
#' @param upper_cyclength_bound Numeric upper bound of cycle lengths to include in scaling. Default is 35.
#'
#' @return The original data frame with the following additional columns:
#' \itemize{
#'   \item \code{scaled_cycleday}: A continuous cycle time variable centered on menses onset (`menses == 1` → 0), ranging from -1 (start of luteal phase) to +1 (ovulation). Only includes cycles with biomarker-confirmed ovulation.
#'   \item \code{scaled_cycleday_impute}: Same as above, but includes cycles where ovulation was imputed using day -15. Offers broader coverage across the dataset, at the cost of lower precision.
#'   \item \code{scaled_cycleday_ov}: A cycle time variable centered on ovulation day (`ovtoday == 1` → 0), ranging from -1 (start of follicular phase) to +1 (end of luteal phase). Only includes cycles with confirmed ovulation.
#'   \item \code{scaled_cycleday_imp_ov}: Same as above, but uses imputed ovulation (`ovtoday_impute == 1`) for cycles lacking biomarker confirmation. Centered on either confirmed or imputed ovulation.
#'   \item \code{ovtoday_impute}: A binary column indicating imputed ovulation days (value `1`) for cycles without confirmed ovulation, estimated as 15 days before menses onset.
#' }
#'
#' @keywords menstrual cycle, ovulation, cycle phase, scaling, time-varying covariate
#' @export
#'
#' @examples 
#' # Load the example dataset
#' cycle_df = cycledata
#'
#' data_with_scaling <- pacts_scaling(
#'   cycle_df, 
#'   id = id, 
#'   date = daterated, 
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
  `%>%` <- magrittr::`%>%`
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
