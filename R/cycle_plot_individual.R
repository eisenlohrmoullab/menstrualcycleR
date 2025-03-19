#' Generate Cycle-Specific Plots and Summary Data for a Given ID
#'
#' This function creates cycle-specific plots and summary statistics for a specified individual (`id`),
#' storing both in a named list for easy access.
#'
#' @param data A dataframe containing menstrual cycle data, including `id` and `cyclenum` columns.
#' @param id The specific individual for whom cycle plots should be generated.
#' @param symptom A string specifying the symptom variable to analyze.
#' @param centering A string indicating the centering phase of the cycle ("menses" or "ovulation").
#' @param y_scale A string specifying the y-axis scale ("person-centered", "person-centered_roll", or "means").
#' @param include_impute A boolean indicating whether to use imputed cycle time values.
#'
#' @return A list where each cycle contains:
#' - `$plot`: The cycle-specific ggplot object with the ID displayed
#' - `$summary`: The corresponding summary data (`dat_summary`)
#' @export
#'
#' @examples
#' data(cycledata)
#' data = calculate_mcyclength(data, id, daterated, menses, ovtoday)
#' data = calculate_cycletime(data, id, daterated, menses, ovtoday)
#' result <- cycle_plot_individual(
#'   data,
#'   id = 1,
#'   "symptom",
#'   centering = "menses",
#'   include_impute = TRUE,
#'   y_scale = "person-centered_roll"
#' )
#' 


cycle_plot_individual <- function(data, id, symptom, centering = "menses", 
                                  y_scale = "person-centered_roll", 
                                  include_impute = TRUE, rollingavg = 5) {
  `:=` <- rlang::`:=`
  `%>%` <- magrittr::`%>%`
  
  # Ensure 'id' and 'cyclenum' columns exist
  if (!"id" %in% colnames(data) | !"cyclenum" %in% colnames(data)) {
    stop("Error: The dataset must contain 'id' and 'cyclenum' columns.")
  }
  
  # Filter data for the specified ID
  data <- data %>% filter(id == !!id)
  
  # Check if data is available for the specified ID
  if (nrow(data) == 0) {
    stop(paste("Error: No data found for ID", id))
  }
  
  # Function to process cycle data
  process_cycle_data <- function(df, symptom) {
    df <- df %>%
      dplyr::mutate((!!dplyr::sym(symptom)) := as.numeric(!!dplyr::sym(symptom)))
    
    forwardCount <- function(x) {
      # Get the indices where mensdayonefirst == 1 (cycle starts)
      inds <- which(x == 1)
      if (!length(inds)) return(rep(NA, length(x)))
      
      # Initialize a numeric vector to store the day count
      num <- rep(NA, length(x))
      
      # Loop through each cycle start index and count forward
      for (i in seq_along(inds)) {
        # Assign 0 to the cycle start day
        num[inds[i]] <- 0
        
        # Define the range to count forward up to +27 or until the next cycle
        cycle_end <- ifelse(i < length(inds), inds[i + 1] - 1, length(x))
        count_range <- (inds[i] + 1):min(cycle_end, inds[i] + (df$mcyclength - 1))
        
        # Assign values +1, +2, ..., up to +27 for the days following the cycle start
        num[count_range] <- seq(1, length(count_range), by = 1)
      }
      
      return(num)
    }
    # Apply the cycleCount function to each person in the dataset
    df$forwardcount <- ave(df$menses, df$id, FUN = forwardCount)  
    
    
    # Create person mean and deviation
    df <- df %>%
      dplyr::mutate(
        "{symptom}.m" := mean(!!dplyr::sym(symptom), na.rm = TRUE),
        "{symptom}.d" := !!dplyr::sym(symptom) - !!dplyr::sym(paste0(symptom, ".m"))
      ) %>%
      dplyr::ungroup()
    
    # Create rolling deviation
    df <- df %>%
      dplyr::mutate(
        "{symptom}.d.roll" := if (sum(!is.na(!!dplyr::sym(paste0(symptom, ".d")))) < 2) {
          NA_real_
        } else {
          zoo::rollapply(!!dplyr::sym(paste0(symptom, ".d")), rollingavg, mean, align = "center", fill = NA)
        }
      ) %>%
      dplyr::ungroup()
    
    # Create rolling mean
    df <- df %>%
      dplyr::mutate(
        "{symptom}.m.roll" := if (sum(!is.na(!!dplyr::sym(paste0(symptom, ".m")))) < 2) {
          NA_real_
        } else {
          zoo::rollapply(!!dplyr::sym(paste0(symptom, ".m")), rollingavg, mean, align = "center", fill = NA)
        }
      ) %>%
      dplyr::ungroup()
    
    # Create rolling sx
    df <- df %>%
      dplyr::mutate(
        "{symptom}.roll" := if (sum(!is.na(!!dplyr::sym(paste0(symptom)))) < 2) {
          NA_real_
        } else {
          zoo::rollapply(!!dplyr::sym(paste0(symptom)), rollingavg, mean, align = "center", fill = NA)
        }
      ) %>%
      dplyr::ungroup()
    
    # Add cycle percentage variable
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
  
  # Function to create a plot and summary data for a given cycle
  create_cycle_plot <- function(df, symptom, cycle, id) {
    # Summarize data for plotting
    dat_summary <- df %>%
      dplyr::group_by(cycleday_5perc) %>%
      dplyr::summarise(
        mean_dev = mean(!!dplyr::sym(paste0(symptom, ".d")), na.rm = TRUE),
        mean_dev_roll = mean(!!dplyr::sym(paste0(symptom, ".d.roll")), na.rm = TRUE),
        raw_sx = mean(!!dplyr::sym(symptom), na.rm = TRUE),
        sx_roll = mean(!!dplyr::sym(paste0(symptom, ".roll")), na.rm = TRUE),
        cycleday = (mean(forwardcount, na.rm = TRUE) + 1),
        mcyclength = first(mcyclength),  # Ensure mcyclength is retained
        .groups = "drop"
      ) 
    
    # X-axis breaks and labels
    x_breaks <- seq(0, 1, by = 0.05)
    x_labels <- if (centering == "menses") {
      c("0%L", rep("", 4), "50%L", rep("", 4), "Menses Onset", rep("", 4), "50%F", rep("", 4), "Ovulation")
    } else {
      c("Menses Onset", rep("", 4), "50%F", rep("", 4), "Ovulation", rep("", 4), "50%L", rep("", 4), "100%L")
    }
    
    # Generate the plot with ID in the title
    plot <- ggplot2::ggplot(dat_summary, ggplot2::aes(
      x = cycleday_5perc,
      y = case_when(
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
        title = paste("Cycle", cycle, "for ID:", id),
        x = "Percentage of Phase Elapsed",
        y = y_scale
      ) +
      ggplot2::theme_minimal()
    
    return(list(plot = plot, summary = dat_summary))
  }
  
  # Get unique cycle numbers for the specified ID
  cycle_nums <- unique(data$cyclenum)
  
  # Initialize an empty list to store results
  results <- list()
  
  # Loop through each cycle and generate plots & summaries
  for (cycle in cycle_nums) {
    cycle_data <- data %>% filter(cyclenum == cycle)
    processed_data <- process_cycle_data(cycle_data, symptom)
    results[[paste0("Cycle_", cycle)]] <- create_cycle_plot(processed_data, symptom, cycle, id)
  }
  
  return(results)
}
