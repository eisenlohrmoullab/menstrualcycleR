library(shiny)
library(menstrualcycleR)
library(dplyr)
library(rlang)
library(ggplot2)  # Ensure ggplot2 is loaded for saving plots

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
    
    # Ensure date column is properly formatted
    if (!inherits(data[[input$date_col]], "Date")) {
      data[[input$date_col]] <- lubridate::ymd(data[[input$date_col]])
    }
    
    # Apply menstrualcycleR functions
    processed <- data %>%
      menstrualcycleR::calculate_mcyclength(
        id = data[[input$id_col]],
        daterated = data[[input$date_col]],
        menses = data[[input$menses_col]],
        ovtoday = data[[input$ovtoday_col]]
      ) %>%
      menstrualcycleR::calculate_cycletime(
        id = data[[input$id_col]],
        daterated = data[[input$date_col]],
        menses = data[[input$menses_col]],
        ovtoday = data[[input$ovtoday_col]],
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
  observeEvent(input$update_individual_plot, {
    req(processed_data(), input$id_selected, input$symptom_cols_individual, input$individual_y_scale, input$individual_rollingavg)
    
    id_selected <- as.numeric(input$id_selected)
    symptoms_selected <- input$symptom_cols_individual
    
    results <- list()
    
    for (symptom in symptoms_selected) {
      plot_results <- menstrualcycleR::cycle_plot_individual(
        data = processed_data(),
        id = id_selected,
        symptom = symptom,
        centering = input$plot_centering_individual,
        include_impute = input$plot_impute_individual,
        y_scale = input$individual_y_scale,
        rollingavg = as.numeric(input$individual_rollingavg)
      )
      
      results[[symptom]] <- plot_results
    }
    
    output$individual_cycle_plots <- renderUI({
      plot_list <- list()
      
      for (symptom in names(results)) {
        for (cycle in names(results[[symptom]])) {
          plot_id <- paste0("plot_", symptom, "_", cycle)
          summary_id <- paste0("summary_", symptom, "_", cycle)
          download_id <- paste0("download_summary_", symptom, "_", cycle)
          
          output[[plot_id]] <- renderPlot({
            req(results[[symptom]][[cycle]]$plot)
            results[[symptom]][[cycle]]$plot
          })
          
          output[[summary_id]] <- renderTable({
            req(results[[symptom]][[cycle]]$summary)
            results[[symptom]][[cycle]]$summary
          })
          
          output[[download_id]] <- downloadHandler(
            filename = function() { paste0("summary_", symptom, "_", cycle, "_", Sys.Date(), ".csv") },
            content = function(file) {
              req(results[[symptom]][[cycle]]$summary)
              write.csv(results[[symptom]][[cycle]]$summary, file, row.names = FALSE)
            }
          )
          
          plot_list <- append(plot_list, list(
            h3(paste("Cycle", cycle, "for", symptom)),
            plotOutput(plot_id),
            actionButton(summary_id, paste("View Summary for", symptom, "Cycle", cycle)),
            downloadButton(download_id, paste("Download Summary for", symptom, "Cycle", cycle)),
            hr()
          ))
        }
      }
      
      do.call(tagList, plot_list)
    })
  })
}
