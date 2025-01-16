#' A function to provide information regarding ovulation confirmation/imputation
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
#' data = calculate_mcyclength(data, id, daterated, menses, ovtoday)
#' data = calculate_cycletime(data, id, daterated, menses, ovtoday)
#' summary_ovulation(data)
#' 

summary_ovulation <- function(data){
  `%>%` <- magrittr::`%>%`
  ovstatus_total <- data.frame(
    "Total Confirmed Ovulation" = sum(data$ovtoday == 1, na.rm = TRUE),
    "Total Imputed Ovulation via Natural Cycles Norms" = data %>%
      dplyr::group_by(id, .data$cyclenum) %>%
      dplyr::filter(all(.data$ovtoday == 0) & any(.data$ovtoday_impute == 1)) %>%
      dplyr::summarise(count = 1, .groups = "drop") %>%
      dplyr::summarise(total = sum(count)) %>%
      dplyr::pull(total),
    row.names = "N cycles",
    check.names = FALSE
  )
  
  
  ovstatus_id <- data %>%
    dplyr::filter(.data$cycle_incomplete == 0) %>%
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
      `Total cycles with cycle length < 21 or > 35` = ifelse(all(.data$mcyclength < 21  &
                                                                   .data$mcyclength > 35), 1, 0),
      `Total cycles with confirmed ovulation` = sum(confirmed_ovulation),
      `Total cycles with imputed ovulation via Natural Cycles Norms` = sum(imputed_ovulation),
      .groups = "drop"
    )
  
  return(list(
    ovstatus_total = ovstatus_total,
    ovstatus_id = ovstatus_id
  ))
  
  print(ovstatus_total)
  print(ovstatus_id)
  
}
