#' Summarize Ovulation Confirmation and Imputation
#'
#' This function summarizes how ovulation was identified in the dataset—either through direct confirmation using a biomarker or via imputation based on cycle timing.
#' Specifically, it counts how often ovulation was directly confirmed (i.e., a value of `1` in the `ovtoday` column, indicating use of biomarkers such as LH surge tests or basal body temperature) versus how often ovulation was estimated using the backward-count method (15 days before the next menses onset).
#' 
#' The output includes both overall counts and per-individual (`id`) summaries.
#'
#' This information is important to examine in your dataset and should be transparently reported in publications. It helps readers understand the degree to which ovulation timing was determined using objective biomarkers (e.g., LH tests or BBT) versus estimated based on assumptions about luteal phase length.
#' Biomarker-based confirmation is preferred whenever possible, as it provides more precise and biologically valid identification of ovulation, thereby improving the accuracy of cycle phase alignment and reducing potential sources of error.
#'
#'
#' @keywords menstrual cycle
#' 
#' @param data A dataframe containing the input data. Must include columns for scaled cycle day variables and symptoms/dependent variables of interest. 
#' Scaled cycle day variables are added to your dataframe after applying the calculate_mcyclength() and calculate_cycletime() functions. (See examples below)
#' 
#' 
#' @return A list containing two data frames: `ovstatus_total` and `ovstatus_id`.
#' 
#' - **`ovstatus_total`**: Among complete cycles, this data frame summarizes:
#'   1. The number of cycles with confirmed ovulation.
#'   2. The number of cycles with ovulation imputed based on Natural Cycles norms.
#' 
#' - **`ovstatus_id`**: For each unique ID, this data frame provides details for complete cycles:
#'   1. The number of cycles outside the 21–35 day range.
#'   2. The number of cycles with confirmed ovulation.
#'   3. The number of cycles with imputed ovulation based on Natural Cycles norms.
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
#' summary_ovulation(data_with_scaling)
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
