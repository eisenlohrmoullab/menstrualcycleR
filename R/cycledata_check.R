#' Analyze Non-Missing Symptom Data by Cycle Phase
#'
#' This function provides detailed information about the availability of non-missing symptom data, stratified by 
#' cycle phases (luteal and follicular) and overall availability. It also generates visualizations to aid in data 
#' quality assessment.
#'
#' @keywords menstrual cycle, data quality, symptoms, cycle phase
#'
#' @param data A data frame containing the input data. 
#'   - Must include columns for scaled cycle day variables and the symptom or dependent variables of interest. 
#'   - Scaled cycle day variables are added to your data frame after applying the `calculate_mcyclength()` and 
#'     `calculate_cycletime()` functions. (See examples below.)
#' @param symptom_columns A character vector of column names representing the dependent variables of interest. 
#'   For example, `c("symptom1", "symptom2", "symptom3")`, where each name corresponds to a column in your data set.
#'
#' @return A list containing three components:
#' - **`by_id`**: A data frame summarizing non-missing values for each ID and symptom, including:
#'   1. Total number of non-missing values.
#'   2. Number of non-missing values during the luteal phase.
#'   3. Number of non-missing values during the follicular phase.
#'
#' - **`overall`**: A data frame summarizing non-missing values for each symptom, including:
#'   1. Total number of non-missing values.
#'   2. Number of non-missing values during the luteal phase.
#'   3. Number of non-missing values during the follicular phase.
#'
#' - **Plots**: Visualizations showing the availability of non-missing data for each symptom, stratified by ID.
#'
#' @export
#'
#' @examples 
#' # Load example dataset
#' data(cycledata)
#'
#' # Calculate menses-to-menses cycle lengths
#' data <- calculate_mcyclength(
#'   data, 
#'   id = id, 
#'   daterated = daterated, 
#'   menses = menses, 
#'   ovtoday = ovtoday
#' )
#'
#' # Add cycle time variables
#' data <- calculate_cycletime(
#'   data, 
#'   id = id, 
#'   daterated = daterated, 
#'   menses = menses, 
#'   ovtoday = ovtoday
#' )
#'
#' # Analyze symptom data availability
#' result <- cycledata_check(
#'   data, 
#'   symptom_columns = c("symptom")
#' )
#'
#' # View results
#' print(result$by_id)
#' print(result$overall)
#' print(result$data_symptom_plots$symptom)


cycledata_check <- function(data, symptom_columns) {
  `%>%` <- magrittr::`%>%`
  
  # Table grouped by id
  result_by_id <- data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(symptom_columns),
        list(
          nonNA = ~ sum(!is.na(.) & !is.na(.data$scaled_cycleday_impute)),
          luteal = ~ sum(!is.na(.) & !is.na(.data$scaled_cycleday_impute) & .data$scaled_cycleday_impute < 0),
          follicular = ~ sum(!is.na(.) & !is.na(.data$scaled_cycleday_impute) & .data$scaled_cycleday_impute >= 0)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
  
  # Check for warnings
  for (symptom in symptom_columns) {
    symptom_nonNA_col <- paste0(symptom, "_nonNA")  # Match the dynamically generated name
    warnings <- result_by_id %>%
      dplyr::filter(.data[[symptom_nonNA_col]] < 10) %>%  # Correctly reference the column
      dplyr::select(id)
    
    if (nrow(warnings) > 0) {
      for (warning_id in warnings$id) {
        message(sprintf("Warning: ID number %s has < 10 observations for %s", warning_id, symptom))
      }
    }
  }
  
  # Overall table (not grouped by id)
  overall_summary <- data %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(symptom_columns),
        list(
          nonNA = ~ sum(!is.na(.) & !is.na(.data$scaled_cycleday_impute)),
          luteal = ~ sum(!is.na(.) & !is.na(.data$scaled_cycleday_impute) & .data$scaled_cycleday_impute < 0),
          follicular = ~ sum(!is.na(.) & !is.na(.data$scaled_cycleday_impute) & .data$scaled_cycleday_impute >= 0)
        ),
        .names = "{.col}_{.fn}"
      )
    )
  
  # Generate plots with lightly colored circles and custom annotations
  data_symptom_plots <- list()
  for (symptom in symptom_columns) {
    filtered_data <- data %>%
      dplyr::filter(
        !is.na(.data[[symptom]]) & !is.na(.data$scaled_cycleday_impute)
      )
    
    symptom_plot <- filtered_data %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$scaled_cycleday_impute, y = factor(id), color = as.numeric(id), fill = as.numeric(id))) +
      ggplot2::geom_point(shape = 21, size = 3, alpha = 0.5, stroke = 0.2) +  
      ggplot2::scale_fill_gradientn(colors = viridis::viridis(256)) +  
      ggplot2::scale_color_gradientn(colors = viridis::viridis(256)) +  
      ggplot2::scale_x_continuous(
        breaks = c(-1, -0.5, 0, 0.5, 1),
        labels = c("0% L", "50% L \nLuteal Phase", "Menses", "50% F \nFollicular Phase", "Ovulation")
      ) +  
      ggplot2::scale_y_discrete(
        limits = levels(filtered_data$id),  # Ensure ID labels are maintained
        labels = function(x) x  # Explicitly set labels to display
      ) +
      ggplot2::labs(
        title = paste("Data Availability for", symptom),
        x = "Cycle Time (Including Imputed Ovulation from NC Norms)",
        y = "ID",
        fill = "ID",
        color = "ID"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 8),
        axis.title = ggplot2::element_text(size = 10),
        plot.title = ggplot2::element_text(size = 12, face = "bold"),
        legend.position = "none"  # Remove the legend
      )
    
    data_symptom_plots[[symptom]] <- symptom_plot
  }
  
  
  # Return a list with both tables and circle-only plots
  return(list(
    by_id = result_by_id,
    overall = overall_summary,
    data_symptom_plots = data_symptom_plots
  ))
}
