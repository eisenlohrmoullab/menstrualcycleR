library(shiny)
library(menstrualcycleR)
library(dplyr)
library(rlang)
library(ggplot2)
library(shinyjs)

suppressMessages({
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  if (!requireNamespace("geomnet", quietly = TRUE)) remotes::install_github("sctyner/geomnet")
  if (!requireNamespace("HiddenSemiMarkov", quietly = TRUE)) devtools::install_github("lasy/HiddenSemiMarkov")
  if (!requireNamespace("cpass", quietly = TRUE)) devtools::install_github("lasy/cpass", dependencies = TRUE)
})

library(cpass)
utils::globalVariables(c("cyclenum", "menses", "symptom", "item", "subject"))

cpass_process <- function(dataframe, symptom_map, id_number) {
  library(dplyr)
  library(tidyr)
  library(cpass)
  
  dataframe <- dataframe %>%
    mutate(subject = id, cycle = cyclenum)
  
  cycleCount <- function(x) {
    inds <- which(x == 1)
    if (!length(inds)) return(0)
    num <- lapply(inds, function(i) {
      num <- seq_along(x) - i
      num[num >= 0] <- num[num >= 0] + 1
      num[num < -15 | num > 10] <- NA
      num
    })
    do.call(coalesce, num)
  }
  
  dataframe <- dataframe %>%
    group_by(id) %>%
    mutate(day = cycleCount(menses)) %>%
    ungroup()
  
  df1_long <- dataframe %>%
    pivot_longer(
      cols = all_of(names(symptom_map)),
      names_to = "symptom",
      values_to = "drsp_score"
    )
  
  df1_long <- df1_long %>%
    mutate(item = recode(as.character(symptom), !!!symptom_map, .default = NA_real_)) %>%
    filter(!is.na(cycle), !is.na(item))
  
  input1 <- as_cpass_data(df1_long, sep_event = "menses")
  
  result <- plot_subject_data_and_dx(data = input1 %>% filter(subject == id_number), save_as_pdf = FALSE)
  
  return(result)
}



server <- function(input, output, session) {
  user_data <- reactiveVal(NULL)
  
  observeEvent(input$load_data, {
    req(input$file)
    data <- read.csv(input$file$datapath)
    user_data(data)
    
    updateSelectInput(session, "id_col", choices = names(data))
    updateSelectInput(session, "date_col", choices = names(data))
    updateSelectInput(session, "menses_col", choices = names(data))
    updateSelectInput(session, "ovtoday_col", choices = names(data))
    updateSelectInput(session, "symptom_col_plot", choices = names(data))
  })
  
  observeEvent(input$id_col, {
    req(user_data(), input$id_col)
    updateSelectInput(session, "selected_id", choices = unique(user_data()[[input$id_col]]))
  })
  
  output$data_preview <- renderTable({ req(user_data()); head(user_data()) })
  
  processed_data <- reactiveVal(NULL)
  
  observeEvent(input$process_data, {
    req(user_data(), input$id_col, input$date_col, input$menses_col, input$ovtoday_col, input$lower_bound, input$upper_bound)
    
    data <- user_data()
    id_col <- sym(input$id_col)
    date_col <- sym(input$date_col)
    menses_col <- sym(input$menses_col)
    ovtoday_col <- sym(input$ovtoday_col)
    
    if (!inherits(data[[input$date_col]], "Date")) {
      data[[input$date_col]] <- lubridate::ymd(data[[input$date_col]])
    }
    
    processed <- data %>%
      menstrualcycleR::calculate_mcyclength(
        id = !!id_col,
        daterated = !!date_col,
        menses = !!menses_col,
        ovtoday = !!ovtoday_col
      ) %>%
      menstrualcycleR::calculate_cycletime(
        id = !!id_col,
        daterated = !!date_col,
        menses = !!menses_col,
        ovtoday = !!ovtoday_col,
        lower_cyclength_bound = as.numeric(input$lower_bound),
        upper_cyclength_bound = as.numeric(input$upper_bound)
      )
    
    processed_data(processed)
  })
  
  output$cycle_data <- renderTable({ req(processed_data()); head(processed_data()) })
  output$cycle_summary <- renderPrint({ req(processed_data()); summary(processed_data()) })
  
  ovulation_summary <- reactive({ req(processed_data()); menstrualcycleR::summary_ovulation(processed_data()) })
  output$ovulation_summary <- renderTable({ req(ovulation_summary()); ovulation_summary()$ovstatus_total })
  output$ovulation_summary_id <- renderTable({ req(ovulation_summary()); ovulation_summary()$ovstatus_id })
  
  cycle_plot_data <- reactiveVal(NULL)
  
  observeEvent(input$update_plot, {
    req(processed_data(), input$symptom_col_plot, input$plot_centering, input$plot_impute, input$rollingavg)
    
    plot_result <- menstrualcycleR::cycle_plot(
      data = processed_data(),
      symptom = input$symptom_col_plot,
      centering = input$plot_centering,
      include_impute = input$plot_impute,
      y_scale = input$plot_y_scale,
      rollingavg = as.numeric(input$rollingavg)
    )
    
    cycle_plot_data(plot_result$plot)
  })
  
  output$cycle_plot <- renderPlot({ req(cycle_plot_data()); cycle_plot_data() })
  output$download_plot <- downloadHandler(
    filename = function() { paste("cycle_plot_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      ggsave(file, plot = cycle_plot_data(), device = "png", width = 8, height = 6, dpi = 300)
    }
  )
  
  observeEvent(processed_data(), {
    updateSelectInput(session, "selected_id", choices = unique(processed_data()$id))
    symptom_choices <- setdiff(names(processed_data()), c("id", "cyclenum", "menses", "scaled_cycleday", "scaled_cycleday_impute", "scaled_cycleday_ov", "scaled_cycleday_imp_ov"))
    updateCheckboxGroupInput(session, "selected_symptoms", choices = symptom_choices)
  })
  
  observeEvent(input$run_individual_plot, {
    req(processed_data(), input$selected_id, input$selected_symptoms)
    
    results <- menstrualcycleR::cycle_plot_individual(
      data = processed_data(),
      id = input$selected_id,
      symptoms = input$selected_symptoms,
      centering = input$centering_mode,
      y_scale = input$y_scale_mode,
      include_impute = input$include_impute_toggle,
      rollingavg = as.numeric(input$rollingavg_input)
    )
    
    output$individual_plot_output <- renderUI({
      output_list <- list()
      
      for (symptom in names(results)) {
        for (cycle_name in names(results[[symptom]])) {
          plot_id <- paste0("plot_", symptom, "_", cycle_name)
          summary_id <- paste0("summary_", symptom, "_", cycle_name)
          toggle_button_id <- paste0("toggle_", summary_id)
          download_summary_id <- paste0("download_", summary_id)
          download_plot_id <- paste0("download_plot_", symptom, "_", cycle_name)
          
          local({
            s <- symptom
            c <- cycle_name
            p_id <- plot_id
            s_id <- summary_id
            d_id <- download_summary_id
            dp_id <- download_plot_id
            toggle_id <- paste0(s_id, "_container")
            
            output[[p_id]] <- renderPlot({ results[[s]][[c]]$plot })
            output[[s_id]] <- renderTable({ results[[s]][[c]]$summary })
            
            output[[d_id]] <- downloadHandler(
              filename = function(){paste0("summary_", s, "_", c, ".csv")},
              content = function(file) {
                write.csv(results[[s]][[c]]$summary, file, row.names = FALSE)
              }
            )
            
            output[[dp_id]] <- downloadHandler(
              filename = function(){paste0("plot_", s, "_", c, ".png")},
              content = function(file) {
                ggsave(file, plot = results[[s]][[c]]$plot, device = "png", width = 8, height = 6, dpi = 300)
              }
            )
            
            observeEvent(input[[toggle_button_id]], {
              shinyjs::toggle(toggle_id)
            })
          }) #closes local 
          
          output_list[[length(output_list) + 1]] <- tagList(
            tags$h4(paste("Symptom:", symptom, "|", cycle_name)),
            plotOutput(plot_id),
            downloadButton(download_plot_id, "Download Plot"),
            actionButton(toggle_button_id, "View Summary"),
            hidden(div(id = paste0(summary_id, "_container"), tableOutput(summary_id))),
            downloadButton(download_summary_id, "Download Summary"),
            tags$hr()
          )
        } #closes cycle name in names 
      } #closes symptom in names
      
      do.call(tagList, output_list)
    }) # closes render UI
  }) # closes input$run_individual_plot observer
  
  
  observeEvent(processed_data(), { #unmatched parentheses error
    req(processed_data())
    req(input$id_col)
    
    updateSelectInput(session, "cpass_id_select", choices = unique(processed_data()[[input$id_col]]))
    
    req(input$id_col, input$date_col, input$menses_col, input$ovtoday_col)
    
    symptom_candidates <- setdiff(
      names(processed_data()),
      c(input$id_col, input$date_col, input$menses_col, input$ovtoday_col,
        "cyclenum", "scaled_cycleday", "scaled_cycleday_impute",
        "scaled_cycleday_ov", "scaled_cycleday_imp_ov", "menses", "ovtoday")
    )
    
    symptom_options <- setNames(as.character(1:21), c(
      "1-Depressed", "2-Hopeless", "3-Worthless/Guilty", "4-Anxious", "5-Mood Swings",
      "6-Rejection Sensitivity", "7-Anger/Irritability", "8-Interpersonal Conflict", "9-Anhedonia",
      "10-Difficulty Concentrating", "11-Low Energy", "12-Overeating", "13-Food Cravings",
      "14-Hypersomnia", "15-Insomnia", "16-Overwhelm", "17-Out of control", "18-Breast Tenderness",
      "19-Bloated/weight gain", "20-Headache", "21-Joint/Muscle Pain"
    ))
    
    output$cpass_mapping_table <- renderUI({
      tagList(
        lapply(symptom_candidates, function(symptom) {
          selectInput(
            inputId = paste0("map_", symptom),
            label = paste0(symptom, ":"),
            choices = c("" = "", symptom_options),
            selected = ""
          )
        })
      )
    })
  })
  
  observeEvent(input$run_cpass, {
    req(processed_data(), input$cpass_id_select)
    
    symptom_inputs <- names(input)[grepl("^map_", names(input))]
    selected_map <- lapply(symptom_inputs, function(x) input[[x]])
    names(selected_map) <- gsub("^map_", "", symptom_inputs)
    selected_map <- selected_map[!vapply(selected_map, function(x) is.null(x) || x == "", logical(1))]
    numeric_map <- setNames(as.numeric(selected_map), names(selected_map))
    
    result <- tryCatch({
      cpass_process(
        dataframe = processed_data(),
        symptom_map = numeric_map,
        id_number = as.numeric(input$cpass_id_select)
      )
    }, error = function(e) {
      showNotification(paste("CPASS Error:", e$message), type = "error")
      return(NULL)
    })
    
    if (!is.null(result)) {
      output$cpass_plot <- renderPlot({ result })
    }
  })
  
  
  output$download_results <- downloadHandler(
    filename = function() { paste("processed_cycle_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(processed_data(), file, row.names = FALSE) }
  )
}

