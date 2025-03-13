library(shiny)
library(menstrualcycleR)
library(dplyr)
library(rlang)

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
    updateSelectInput(session, "symptom_col", choices = names(data))  # Update symptom selector
  })
  
  # Show Data Preview
  output$data_preview <- renderTable({
    req(user_data())
    head(user_data())
  })
  
  # Reactive Processed Data
  processed_data <- reactiveVal(NULL)
  
  observeEvent(input$process_data, {
    req(user_data(), input$id_col, input$date_col, input$menses_col, input$ovtoday_col)
    
    # Convert input selections to symbols
    id_col <- sym(input$id_col)
    date_col <- sym(input$date_col)
    menses_col <- sym(input$menses_col)
    ovtoday_col <- sym(input$ovtoday_col)
    
    # Apply functions with dynamic column referencing
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
        ovtoday = !!ovtoday_col
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
  
  # Symptom Check
  symptom_check <- reactive({
    req(processed_data(), input$symptom_col)
    
    menstrualcycleR::cycledata_check(
      processed_data(),
      symptom_columns = input$symptom_col  # Pass as a string
    )
  })
  
  output$symptom_check <- renderTable({
    req(symptom_check())
    symptom_check()$overall
  })
  
  # Cycle Plot
  cycle_plot_data <- reactiveVal(NULL)
  
  observeEvent(input$update_plot, {
    req(processed_data(), input$symptom_col, input$plot_centering, input$plot_impute)
    
    symptom_col <- sym(input$symptom_col)
    
    plot_result <- menstrualcycleR::cycle_plot(
      processed_data(),
      symptom = !!symptom_col,  # Use dynamic symptom selection
      centering = input$plot_centering,  # Use user-selected centering
      include_impute = input$plot_impute  # Use user-selected imputation option
    )
    
    cycle_plot_data(plot_result$plot)  # Store the plot
  })
  
  output$cycle_plot <- renderPlot({
    req(cycle_plot_data())
    cycle_plot_data()
  })
  
  # Download Processed Data
  output$download_results <- downloadHandler(
    filename = function() { paste("processed_cycle_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(processed_data(), file, row.names = FALSE) }
  )
}
