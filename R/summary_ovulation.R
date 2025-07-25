#' Summarize Ovulation Confirmation and Imputation
#'
#' This function provides a summary of how ovulation was identified across the dataset—either through direct confirmation using biomarkers or via imputation based on menstrual cycle timing.
#'
#' Specifically, it counts the number of cycles in which ovulation was:
#' - **Confirmed** using objective biomarkers (i.e., a `1` in the `ovtoday` column), such as urinary LH surge tests or basal body temperature (BBT).
#' - **Imputed** using the backward-count method, which estimates ovulation as 15 days before the subsequent menses onset (`ovtoday_impute == 1`), based on the typical length of the luteal phase.
#'
#' The output includes both:
#' - Overall summary counts across the entire dataset.
#' - Per-individual summaries to support participant-level quality checks and reporting.
#'
#' This summary is essential for understanding data quality and should be transparently reported in publications. It helps clarify how often ovulation timing was determined using biologically grounded methods versus estimated through assumptions. Whenever possible, biomarker-based confirmation (e.g., LH or BBT) is preferred, as it provides more precise and physiologically valid estimates of ovulation timing. This enhances the accuracy of cycle phase alignment and reduces potential sources of error in time-sensitive analyses.
#'
#' When biomarkers are unavailable, imputation via the -15 day backward-count method offers a more biologically valid estimate than assuming ovulation occurs at the cycle midpoint. This method accounts for the relative stability of the luteal phase length and is recommended over midpoint-based estimates (see Nagpal et al., 2025).
#'
#' For further guidance on ovulation identification and the rationale for the -15 day imputation method, see:
#' - Nagpal et al. (2025). *Studying the Menstrual Cycle as a Continuous Variable: Implementing Phase-Aligned Cycle Time Scaling (PACTS) with the `menstrualcycleR` package*. https://doi.org/10.31219/osf.io/hd5xw_v1  
#' - Schmalenberger et al. (2021). *How to study the menstrual cycle: Practical tools and recommendations*. *Psychoneuroendocrinology, 123*, 104895. https://doi.org/10.1016/j.psyneuen.2020.104895
#'
#' @keywords menstrual cycle, ovulation, biomarkers, imputation
#'
#' @param data A dataframe containing the input data. Must include columns for scaled cycle day variables and symptoms/dependent variables of interest.
#' Scaled cycle day variables are added to your dataframe after applying the `pacts_scaling()` function. (See examples below)
#'
#' @return A list with two data frames:
#' \itemize{
#'   \item \code{ovstatus_total}: A dataset-level summary showing:
#'   \enumerate{
#'     \item The number of cycles with confirmed ovulation (`ovtoday == 1`).
#'     \item The number of cycles with imputed ovulation (`ovtoday_impute == 1`).
#'   }
#'
#'   \item \code{ovstatus_id}: A participant-level summary showing, for each unique ID:
#'   \enumerate{
#'     \item The number of cycles with confirmed ovulation.
#'     \item The number of cycles with imputed ovulation.
#'   }
#' }
#' 
#'  
#' @export
#' @examples 
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
#' ov_summary = summary_ovulation(data_with_scaling)
#' print(ov_summary)
#' ov_summary$ovstatus_total
#' ov_summary$ovstatus_id
#' 

summary_ovulation <- function(data){
  `%>%` <- magrittr::`%>%`
  ovstatus_total <- data.frame(
    "Total Confirmed Ovulation" = sum(data$ovtoday == 1, na.rm = TRUE),
    "Total Estimated Ovulation via 15day Backward Count" = data %>%
      dplyr::group_by(id, .data$cyclenum) %>%
      dplyr::filter(all(.data$ovtoday == 0) & any(.data$ovtoday_impute == 1)) %>%
      dplyr::summarise(count = 1, .groups = "drop") %>%
      dplyr::summarise(total = sum(count)) %>%
      dplyr::pull(total),
    row.names = "N cycles",
    check.names = FALSE
  )
  
  
  ovstatus_id <- data %>%
    #dplyr::filter(.data$cycle_incomplete != 0) %>%
    dplyr::group_by(id, .data$cyclenum) %>%
    dplyr::summarise(
      # Total cycles with cycle length < 21 or > 35 and ovtoday == 0, ovtoday_impute == 0
      cycles_outside_norm = ifelse(all(.data$mcyclength < 21  &
                                         .data$mcyclength > 35), 1, 0),
      # Total confirmed ovulation: ovtoday == 1 and ovtoday_impute == 0
      confirmed_ovulation = ifelse(
        any(.data$ovtoday == 1) &
          all(.data$ovtoday_impute == 0) |
          any(.data$ovtoday == 1) & any(.data$ovtoday_impute == 1),
        1,
        0
      ),
      
      # Total imputed ovulation: ovtoday == 0 and ovtoday_impute == 1
      imputed_ovulation = ifelse(all(.data$ovtoday == 0) &
                                   any(.data$ovtoday_impute == 1), 1, 0),
      .groups = "drop"
    ) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      #`Total cycles with cycle length < 21 or > 35` = sum(cycles_outside_norm),
      `Total cycles with confirmed ovulation` = sum(confirmed_ovulation),
      `Total cycles with imputed ovulation via 15day Backward Count` = sum(imputed_ovulation),
      .groups = "drop"
    )
  
  return(list(
    ovstatus_total = ovstatus_total,
    ovstatus_id = ovstatus_id
  ))
  
  print(ovstatus_total)
  print(ovstatus_id)
  
}
