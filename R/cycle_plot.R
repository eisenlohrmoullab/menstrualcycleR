#' Plot Symptom Data Across the Menstrual Cycle
#'
#' This function creates a visual representation of symptom data across the menstrual cycle.
#' It calculates mean and deviation values for the symptom by participant, allows centering the visualization on menses or ovulation,
#' and provides flexibility in choosing the y-axis scale for the plot.
#'
#' @param data A dataframe containing the input data. Must include columns for scaled cycle day variables and the symptom variable. 
#' Scaled cycle day variables are added to your dataframe after applying the calculate_mcyclength() and calculate_cycletime() functions. (See examples below)
#' @param symptom A string specifying the symptom variable to analyze.
#' @param centering A string indicating the centering phase of the cycle ("menses" or "ovulation"). Default is "menses".
#' @param include_impute A boolean indicating whether to use imputed cycle time values based on NC cycle length norms. Default is TRUE.
#' @param y_scale A string specifying the y-axis scale. Options are "person-centered", "person-centered_roll", or "means". Default is "person-centered".
#'
#' @return A list containing the following elements:
#' 	1. A dataframe: The input dataframe augmented with person-centered mean (.m) and deviation (.d) values for the symptom variable.
#' 	2. A summary dataframe: Contains the summarized y-axis variable values for each 5% increment of the cycle percentage.
#' 	3. A ggplot object: A plot visualizing the summarized data across the menstrual cycle.
#'
#' @examples
#' # Example usage:
#' data(cycledata)
#' data = calculate_mcyclength(data, id, daterated, menses, ovtoday)
#' data = calculate_cycletime(data, id, daterated, menses, ovtoday)
#' result <- cycle_plot(
#'   data,
#'   "symptom",
#'   centering = "menses",
#'   include_impute = TRUE,
#'   y_scale = "person-centered"
#' )
#' result$plot
#' @export
#' 

cycle_plot <- function(data, symptom, centering = "menses", include_impute = TRUE, y_scale = "person-centered") {
  
  `%>%` <- magrittr::`%>%`
  `:=` <- rlang::`:=`
  data <- data %>%
    dplyr::mutate(symptom = as.numeric(symptom))
  
  # Function to create person means
  create_person_mean <- function(df, var, group_var) {
    df %>%
      dplyr::group_by(!!dplyr::sym(group_var)) %>%
      dplyr::mutate("{{var}}.m" := mean({{var}}, na.rm = TRUE))
  }
  
  # Function to create deviations
  create_deviation <- function(df, var, var_m) {
    df %>%
      dplyr::mutate("{{var}}.d" := as.numeric({{var}}) - as.numeric({{var_m}}))
  }
  
  # Function to create 5-day rolling deviations 
  create_rolling_deviation <- function(df, var.d) {
    df <- df %>%
      dplyr::group_by(id) %>%
      dplyr::mutate("{{var.d}}.roll" := zoo::rollapply({{var.d}}, 5, mean, align = "center", fill = NA)) %>%
      dplyr::ungroup()
  }
  
  # Apply person-mean centering and rolling avg calculations
  data <- create_person_mean(data, !!dplyr::sym(symptom), "id")
  data <- create_deviation(data, !!dplyr::sym(symptom), !!dplyr::sym(paste0(symptom, ".m")))
  data <- create_rolling_deviation(data, !!dplyr::sym(paste0(symptom, ".d")))
  
  # Add cycle percentage variables based on centering and include_impute
  if (centering == "menses" & include_impute) {
    data <- data %>%
      dplyr::mutate(cycleday_perc = (.data$scaled_cycleday + 1) / 2) %>%
      dplyr::mutate(cycleday_5perc = round(cycleday_perc / 0.05) * 0.05)
  } else if (centering == "menses" & !include_impute) {
    data <- data %>%
      dplyr::mutate(cycleday_perc = (.data$scaled_cycleday_impute + 1) / 2) %>%
      dplyr::mutate(cycleday_5perc = round(cycleday_perc / 0.05) * 0.05)
  } else if (centering == "ovulation" & include_impute) {
    data <- data %>%
      dplyr::mutate(cycleday_perc = (.data$scaled_cycleday_imp_ov + 1) / 2) %>%
      dplyr::mutate(cycleday_5perc = round(cycleday_perc / 0.05) * 0.05)
  } else if (centering == "ovulation" & !include_impute) {
    data <- data %>%
      dplyr::mutate(cycleday_perc = (.data$scaled_cycleday_ov + 1) / 2) %>%
      dplyr::mutate(cycleday_5perc = round(cycleday_perc / 0.05) * 0.05)
  }
  
  # Group and summarize data for plotting
  if (y_scale == "person-centered") {
    dat_summary <- data %>%
      dplyr::group_by(cycleday_5perc) %>%
      dplyr::summarise(mean_dev = mean(!!dplyr::sym(paste0(symptom, ".d")), na.rm = TRUE))
  } else if (y_scale == "person-centered_roll") {
    dat_summary <- data %>%
      dplyr::group_by(cycleday_5perc) %>%
      dplyr::summarise(mean_dev_roll = mean(!!dplyr::sym(paste0(symptom, ".d.roll")), na.rm = TRUE)) 
  } else if (y_scale == "means") {
    dat_summary <- data %>%
      dplyr::group_by(cycleday_5perc) %>%
      dplyr::summarise(mean_sx = mean(!!dplyr::sym(paste0(symptom, ".m")), na.rm = TRUE))
  }
  
  # X-axis breaks and labels
  x_breaks <- seq(0, 1, by = 0.05)
  x_labels <- if (centering == "menses") {
    c("0%L", rep("", 4), "50%L", rep("", 4), "Menses Onset", rep("", 4), "50%F", rep("", 4), "Ovulation")
  } else {
    c("Menses Onset", rep("", 4), "50%F", rep("", 4), "Ovulation", rep("", 4), "50%L", rep("", 4), "100%L")
  }
  
  # Create plot
  plot <- ggplot2::ggplot(dat_summary, ggplot2::aes(
    x = cycleday_5perc,
    y = if (y_scale == "person-centered") mean_dev else if (y_scale == "person-centered_roll") mean_dev_roll else mean_sx
  )) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = x_breaks, labels = x_labels) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = ifelse(centering == "ovulation", 0.48, 0.48), xmax = ifelse(centering == "ovulation", 0.52, 0.52), ymin = -Inf, ymax = Inf),
      fill = ifelse(centering == "ovulation", "grey87", "grey70"), alpha = 0.2, color = "white"
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = ifelse(centering == "ovulation", 0, 0.96), xmax = ifelse(centering == "ovulation", 0.04, 1), ymin = -Inf, ymax = Inf),
      fill = "grey70", alpha = 0.2, color = "white"
    ) +
    ggplot2::geom_line(size = 0.7) +
    ggplot2::labs(
      x = "Percentage of Phase Elapsed",
      y = paste("Mean", y_scale)
    ) +
    ggplot2::theme_minimal()
  
  return(list(data = data, summary = dat_summary, plot = plot))
}
