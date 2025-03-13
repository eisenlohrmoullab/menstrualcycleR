library(shiny)
library(menstrualcycleR)

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
    
    # Apply the menstrualcycleR functions in order
    processed <- menstrualcycleR::calculate_mcyclength(
      data = user_data(),
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
      ovtoday = input$ovtoday_col
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
  
  # Symptom Data Analysis
  symptom_analysis <- reactive({
    req(processed_data())
    menstrualcycleR::cycledata_check(
      processed_data(),
      symptom_columns = c("symptom")  # Can make dynamic
    )
  })
  
  output$symptom_analysis <- renderTable({
    req(symptom_analysis())
    symptom_analysis()$overall
  })
  
  # Cycle Plot
  output$cycle_plot <- renderPlot({
    req(processed_data())
    result <- menstrualcycleR::cycle_plot(
      processed_data(),
      symptom = "symptom",
      centering = "menses"
    )
    result$plot
  })
  
  # Download Processed Data
  output$download_results <- downloadHandler(
    filename = function() { paste("processed_cycle_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(processed_data(), file, row.names = FALSE) }
  )
}
