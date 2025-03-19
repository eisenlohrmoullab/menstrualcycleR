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
  
  # Show Data Preview
  output$data_preview <- renderTable({
    req(user_data())
    head(user_data())
  })
  
  # Reactive Processed Data
  processed_data <- reactiveVal(NULL)
  
  observeEvent(input$process_data, {
    req(user_data(), input$id_col, input$date_col, input$menses_col, input$ovtoday_col, input$lower_bound, input$upper_bound)
    
    # Get the data
    data <- user_data()
    
    # Validate data before processing
    validate(need(!is.null(data), "No data loaded. Please upload a file."))
    
    # Apply menstrualcycleR functions **without renaming columns**
    processed <- menstrualcycleR::calculate_mcyclength(
      data = data,
      id = input$id_col,
      daterated = input$date_col,
      menses = input$menses_col,
      ovtoday = input$ovtoday_col
    )
    
    processed <- menstrualcycleR::calculate_cycletime(
      data = processed,
      id = input$id_col,
      daterated = input$date_col,
      menses = input$menses_col,
      ovtoday = input$ovtoday_col,
      lower_cyclength_bound = input$lower_bound,
      upper_cyclength_bound = input$upper_bound
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
  
  # Cycle Plot
  cycle_plot_data <- reactiveVal(NULL)
  
  observeEvent(input$update_plot, {
    req(processed_data(), input$symptom_col_plot, input$plot_centering, input$plot_impute, input$rollingavg)
    
    plot_result <- menstrualcycleR::cycle_plot(
      processed_data(),
      symptom = input$symptom_col_plot,
      centering = input$plot_centering,
      include_impute = input$plot_impute,
      y_scale = input$plot_y_scale,
      rollingavg = input$rollingavg
    )
    
    cycle_plot_data(plot_result$plot)
  })
  
  output$cycle_plot <- renderPlot({
    req(cycle_plot_data())
    cycle_plot_data()
  })
  
  # Download Cycle Plot
  output$download_plot <- downloadHandler(
    filename = function() { paste("cycle_plot_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      req(cycle_plot_data())
      ggsave(file, plot = cycle_plot_data(), device = "png", width = 8, height = 6, dpi = 300)
    }
  )
  
  # Individual Cycle Plots
  output$individual_cycle_plots <- renderUI({
    req(processed_data(), input$id_selected, input$symptom_cols_individual)
    
    id_selected <- input$id_selected
    symptoms_selected <- input$symptom_cols_individual
    
    plot_list <- list()
    
    for (symptom in symptoms_selected) {
      plot_results <- menstrualcycleR::cycle_plot_individual(
        data = processed_data(),
        id = id_selected,
        symptom = symptom,
        centering = input$plot_centering_individual,
        include_impute = input$plot_impute_individual,
        y_scale = input$individual_y_scale,
        rollingavg = input$individual_rollingavg
      )
      
      for (cycle in names(plot_results)) {
        plot_id <- paste0("plot_", symptom, "_", cycle)
        summary_id <- paste0("summary_", symptom, "_", cycle)
        download_id <- paste0("download_summary_", symptom, "_", cycle)
        
        plot_list[[plot_id]] <- plotOutput(plot_id)
        output[[plot_id]] <- renderPlot({
          req(plot_results[[cycle]]$plot)
          plot_results[[cycle]]$plot
        })
        
        # Add summary table and download buttons
        plot_list[[paste0("btn_", summary_id)]] <- tagList(
          actionButton(summary_id, paste("View Summary for", symptom, "Cycle", cycle)),
          downloadButton(download_id, paste("Download Summary for", symptom, "Cycle", cycle))
        )
        
        output[[summary_id]] <- renderTable({
          req(plot_results[[cycle]]$summary)
          plot_results[[cycle]]$summary
        })
        
        output[[download_id]] <- downloadHandler(
          filename = function() { paste0("summary_", symptom, "_", cycle, "_", Sys.Date(), ".csv") },
          content = function(file) {
            req(plot_results[[cycle]]$summary)
            write.csv(plot_results[[cycle]]$summary, file, row.names = FALSE)
          }
        )
      }
    }
    
    do.call(tagList, plot_list)
  })
  
  # Download Processed Data
  output$download_results <- downloadHandler(
    filename = function() { paste("processed_cycle_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(processed_data(), file, row.names = FALSE) }
  )
}
