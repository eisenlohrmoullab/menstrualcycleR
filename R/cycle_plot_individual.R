#' Generate Cycle-Specific Plots and Summary Data for a Given ID
#'
#' This function creates cycle-specific plots and summary statistics for a specified individual (`id`),
#' storing both in a named list for easy access.
#'
#' @param data A dataframe containing menstrual cycle data, including `id` and `cyclenum` columns.
#' @param id Numeric id number for the specific individual for whom cycle plots should be generated.
#' @param symptoms A vector of strings specifying the symptom variable to analyze that exist in `data`.
#' @param centering A string indicating the centering phase of the cycle ("menses" or "ovulation").
#' @param y_scale A string specifying the y-axis scale ("person-centered", "person-centered_roll", "raw", or "roll").
#' @param include_impute A boolean indicating whether to use imputed cycle time values.
#' @param rollingavg A numeric indicating how many days of a rolling average to use, the default is 5
#'
#' @return A list where each cycle contains:
#' - `$plot`: The cycle-specific ggplot object with the ID displayed
#' - `$summary`: The corresponding summary data (`dat_summary`)
#' @export
#'
#' @examples
#' data("cycledata")
#' data = calculate_mcyclength(data, id = id, daterated , menses = menses, ovtoday = ovtoday)
#' data = calculate_cycletime(data, id = id, daterated , menses = menses, ovtoday = ovtoday)
# results <- cycle_plot_individual(data, id = 1, symptom = "symptom", centering = "menses", y_scale = "person-centered")
#' 
#' 


cycle_plot_individual <- function(data, id, symptoms, centering = "menses", 
                                       y_scale = "person-centered", 
                                       include_impute = TRUE, rollingavg = 5) {
  `:=` <- rlang::`:=`
  `%>%` <- magrittr::`%>%`
  
  # Ensure required columns exist
  required_cols <- c("id", "cyclenum")
  if (!all(required_cols %in% colnames(data))) {
    stop("Error: The dataset must contain 'id' and 'cyclenum' columns.")
  }
  
  # Convert id to numeric (if applicable)
  id <- as.numeric(id)
  
  # Filter data for the specified ID
  data_filtered <- data %>% filter(id == !!id)
  data_filtered = as.data.frame(data_filtered)
  data_filtered <- data_filtered %>% dplyr::filter(!is.na(cyclenum))
  
  # Ensure data is still a dataframe after filtering
  if (nrow(data) == 0) {
    stop(paste("Error: No data found for ID", id))
  }
  
  # Function to process cycle data for each symptom
  process_cycle_data <- function(df, symptom) {
    df <- df %>%
      dplyr::mutate(!!symptom := as.numeric(!!sym(symptom)))
    
    forwardCount <- function(x) {
      inds <- which(x == 1)
      if (length(inds) == 0) return(rep(NA, length(x)))
      
      num <- rep(NA, length(x))
      
      for (i in seq_along(inds)) {
        num[inds[i]] <- 0
        cycle_end <- ifelse(i < length(inds), inds[i + 1] - 1, length(x))
        count_range <- (inds[i] + 1):min(cycle_end, inds[i] + (df$mcyclength - 1))
        num[count_range] <- seq(1, length(count_range), by = 1)
      }
      
      return(num)
    }
    
    df$forwardcount <- ave(df$menses, df$id, FUN = forwardCount)
    
    df <- df %>%
      dplyr::mutate(
        !!paste0(symptom, ".m") := mean(!!sym(symptom), na.rm = TRUE),
        !!paste0(symptom, ".d") := !!sym(symptom) - !!sym(paste0(symptom, ".m"))
      ) %>%
      dplyr::ungroup()
    
    df <- df %>%
      dplyr::mutate(
        !!paste0(symptom, ".d.roll") := if (sum(!is.na(!!sym(paste0(symptom, ".d")))) < 2) {
          NA_real_
        } else {
          zoo::rollapply(!!sym(paste0(symptom, ".d")), rollingavg, mean, align = "center", fill = NA)
        },
        !!paste0(symptom, ".m.roll") := if (sum(!is.na(!!sym(paste0(symptom, ".m")))) < 2) {
          NA_real_
        } else {
          zoo::rollapply(!!sym(paste0(symptom, ".m")), rollingavg, mean, align = "center", fill = NA)
        },
        !!paste0(symptom, ".roll") := if (sum(!is.na(!!sym(symptom))) < 2) {
          NA_real_
        } else {
          zoo::rollapply(!!sym(symptom), rollingavg, mean, align = "center", fill = NA)
        }
      ) %>%
      dplyr::ungroup()
    
    df <- df %>%
      dplyr::mutate(
        cycleday_perc = case_when(
          centering == "menses" & include_impute ~ (.data$scaled_cycleday_impute + 1) / 2,
          centering == "menses" & !include_impute ~ (.data$scaled_cycleday + 1) / 2,
          centering == "ovulation" & include_impute ~ (.data$scaled_cycleday_imp_ov + 1) / 2,
          centering == "ovulation" & !include_impute ~ (.data$scaled_cycleday_ov + 1) / 2
        ),
        cycleday_5perc = round(cycleday_perc / 0.05) * 0.05
      )
    
    return(df)
  }
  
  # Function to create plots for each cycle
  create_cycle_plot <- function(df, symptom, cycle, id) {
    dat_summary <- df %>%
      dplyr::group_by(cycleday_5perc) %>%
      dplyr::summarise(
        mean_dev = mean(!!sym(paste0(symptom, ".d")), na.rm = TRUE),
        mean_dev_roll = mean(!!sym(paste0(symptom, ".d.roll")), na.rm = TRUE),
        raw_sx = mean(!!sym(symptom), na.rm = TRUE),
        sx_roll = mean(!!sym(paste0(symptom, ".roll")), na.rm = TRUE),
        cycleday = mean(forwardcount, na.rm = TRUE) + 1,
        mcyclength = first(mcyclength),
        .groups = "drop"
      )
    
    x_breaks <- seq(0, 1, by = 0.05)
    x_labels <- if (centering == "menses") {
      c("0%L", rep("", 4), "50%L", rep("", 4), "Menses Onset", rep("", 4), "50%F", rep("", 4), "Ovulation")
    } else {
      c("Menses Onset", rep("", 4), "50%F", rep("", 4), "Ovulation", rep("", 4), "50%L", rep("", 4), "100%L")
    }
    
    plot <- ggplot2::ggplot(dat_summary, ggplot2::aes(
      x = cycleday_5perc,
      y = dplyr::case_when(
        y_scale == "person-centered" ~ mean_dev,
        y_scale == "person-centered_roll" ~ mean_dev_roll,
        y_scale == "raw" ~ raw_sx,
        y_scale == "roll" ~ sx_roll
      )
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
        title = paste("Cycle", cycle, "for", symptom, "ID:", id),
        x = "Percentage of Phase Elapsed",
        y = y_scale
      ) +
      ggplot2::theme_minimal()
    
    return(list(plot = plot, summary = dat_summary))
  }
  
  # Get unique cycle numbers
  cycle_nums <- unique(data_filtered$cyclenum)
  
  results <- list()
  
  for (symptom in symptoms) {
    symptom_results <- list()
    
    for (cycle in cycle_nums) {
      cycle_data <- data_filtered %>% dplyr::filter(cyclenum == cycle)
      
      if (nrow(cycle_data) == 0) next  # Skip if no data for this cycle
      
      # Process cycle data
      processed_data <- process_cycle_data(cycle_data, symptom)
      
      # Generate plot and summary
      symptom_results[[paste0("Cycle_", cycle)]] <- create_cycle_plot(processed_data, symptom, cycle, id)
    }
    
    results[[symptom]] <- symptom_results
  }
  
  return(results)
}
