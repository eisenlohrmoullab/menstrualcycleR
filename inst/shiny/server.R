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
    updateCheckboxGroupInput(session, "symptom_cols_individual", "Select Symptoms for Individual Plots:", choices = names(data))
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
    
    # Convert input selections to symbols
    id_col <- sym(input$id_col)
    date_col <- sym(input$date_col)
    menses_col <- sym(input$menses_col)
    ovtoday_col <- sym(input$ovtoday_col)
    
    # Apply functions with user-defined cycle length bounds
    processed <- user_data() %>%
      menstrualcycleR::calculate_mcyclength(
        data = .,
        id = !!id_col,
        daterated = !!date_col,
        menses = !!menses_col,
        ovtoday = !!ovtoday_col
      ) %>%
      menstrualcycleR::calculate_cycletime(
        data = .,
        id = !!id_col,
        daterated = !!date_col,
        menses = !!menses_col,
        ovtoday = !!ovtoday_col,
        lower_cyclength_bound = input$lower_bound,  # User-defined lower bound
        upper_cyclength_bound = input$upper_bound   # User-defined upper bound
      )
    
    processed_data(processed)  # Store processed data
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
  
  # Download Processed Data
  output$download_results <- downloadHandler(
    filename = function() { paste("processed_cycle_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(processed_data(), file, row.names = FALSE) }
  )
}
