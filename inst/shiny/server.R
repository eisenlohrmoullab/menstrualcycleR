library(shiny)
library(menstrualcycleR)
library(dplyr)
library(rlang)
library(ggplot2)  # Ensure ggplot2 is loaded for saving plots
suppressMessages({
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  if (!requireNamespace("geomnet", quietly = TRUE)) remotes::install_github("sctyner/geomnet")
  if (!requireNamespace("HiddenSemiMarkov", quietly = TRUE)) devtools::install_github("lasy/HiddenSemiMarkov")
  if (!requireNamespace("cpass", quietly = TRUE)) devtools::install_github("lasy/cpass", dependencies = TRUE)
})
library(cpass)


server <- function(input, output, session) {
  
  # Reactive Value for User Data
  user_data <- reactiveVal(NULL)
  
  observeEvent(input$load_data, {
    req(input$file)
    data <- read.csv(input$file$datapath)
    user_data(data)
    
    # Update Column Selectors
    updateSelectInput(session, "id_col", choices = names(data))
    updateSelectInput(session, "date_col", choices = names(data))
    updateSelectInput(session, "menses_col", choices = names(data))
    updateSelectInput(session, "ovtoday_col", choices = names(data))
    updateSelectInput(session, "symptom_col_plot", choices = names(data))
    updateCheckboxGroupInput(session, "symptom_cols_individual", choices = names(data))
  })
  
  # Update ID Selection Dropdown when id_col is chosen
  observeEvent(input$id_col, {
    req(user_data(), input$id_col)
    updateSelectInput(session, "id_selected", 
                      choices = unique(user_data()[[input$id_col]]), 
                      selected = unique(user_data()[[input$id_col]])[1])
  })
  
  # Show Data Preview
  output$data_preview <- renderTable({
    req(user_data())
    head(user_data())
  })
  
  # Reactive Processed Data
  processed_data <- reactiveVal(NULL)
  
  observeEvent(input$process_data, {
    req(user_data(), input$id_col, input$date_col, input$menses_col, input$ovtoday_col, input$lower_bound, input$upper_bound)
    
    data <- user_data()
    
    # Convert input selections to symbols
    id_col <- rlang::sym(input$id_col)
    date_col <- rlang::sym(input$date_col)
    menses_col <- rlang::sym(input$menses_col)
    ovtoday_col <- rlang::sym(input$ovtoday_col)
    
    # Ensure date column is properly formatted
    if (!inherits(data[[input$date_col]], "Date")) {
      data[[input$date_col]] <- lubridate::ymd(data[[input$date_col]])
    }
    
    # Apply menstrualcycleR functions **with correct syntax**
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
  
  # Display Processed Data
  output$cycle_data <- renderTable({
    req(processed_data())
    head(processed_data())
  })
  
  output$cycle_summary <- renderPrint({
    req(processed_data())
    summary(processed_data())
  })
  
  # Ovulation Analysis
  ovulation_summary <- reactive({
    req(processed_data())
    menstrualcycleR::summary_ovulation(processed_data())
  })
  
  output$ovulation_summary <- renderTable({
    req(ovulation_summary())
    ovulation_summary()$ovstatus_total
  })
  
  output$ovulation_summary_id <- renderTable({
    req(ovulation_summary())
    ovulation_summary()$ovstatus_id
  })
  
  # **Cycle Plot**
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
  
  output$cycle_plot <- renderPlot({
    req(cycle_plot_data())
    cycle_plot_data()
  })
  
  output$download_plot <- downloadHandler(
    filename = function() { paste("cycle_plot_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      req(cycle_plot_data())
      ggsave(file, plot = cycle_plot_data(), device = "png", width = 8, height = 6, dpi = 300)
    }
  )
  
  # **Individual Cycle Plots**
  observe({
    req(processed_data)
    updateSelectInput(session, "selected_id", choices = unique(processed_data()$id))
    updateCheckboxGroupInput(session, "selected_symptoms",
                             choices = setdiff(names(processed_data), c("id", "cyclenum", "menses", "scaled_cycleday", "scaled_cycleday_impute", "scaled_cycleday_ov", "scaled_cycleday_imp_ov")))
  })
  
  observeEvent(input$run_individual_plot, {
    req(input$selected_id, input$selected_symptoms)
    
    results <- menstrualcycleR::cycle_plot_individual(
      data = processed_data,
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
            
            output[[p_id]] <- renderPlot({
              results[[s]][[c]]$plot
            })
            
            output[[s_id]] <- renderTable({
              results[[s]][[c]]$summary
            })
            
            output[[d_id]] <- downloadHandler(
              filename = function() paste0("summary_", s, "_", c, ".csv"),
              content = function(file) {
                write.csv(results[[s]][[c]]$summary, file, row.names = FALSE)
              }
            )
            
            output[[dp_id]] <- downloadHandler(
              filename = function() paste0("plot_", s, "_", c, ".png"),
              content = function(file) {
                ggsave(file, plot = results[[s]][[c]]$plot, device = "png", width = 8, height = 6, dpi = 300)
              }
            )
          })
          
          output_list[[length(output_list) + 1]] <- tagList(
            tags$h4(paste("Symptom:", symptom, "|", cycle_name)),
            plotOutput(plot_id),
            downloadButton(download_plot_id, "Download Plot"),
            actionButton(toggle_button_id, "View Summary"),
            hidden(div(id = paste0(summary_id, "_container"), tableOutput(summary_id))),
            downloadButton(download_summary_id, "Download Summary"),
            tags$hr()
          )
          
          observeEvent(input[[toggle_button_id]], {
            toggle_id <- paste0("#", summary_id, "_container")
            session$sendCustomMessage(type = "toggleSummary", message = toggle_id)
          })
        }
      }
      
      do.call(tagList, output_list)
    })
  })
  
  session$onFlushed(function() {
    session$sendCustomMessage(type = "initToggleScript", message = NULL)
  })

   
}
