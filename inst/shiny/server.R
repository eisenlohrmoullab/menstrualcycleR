library(shiny)
library(menstrualcycleR)

server <- function(input, output, session) {
  
  # Reactive: Load Data
  user_data <- reactiveVal(NULL)
  
  observeEvent(input$load_data, {
    req(input$file)
    data <- read.csv(input$file$datapath)
    user_data(data)
    
    # Update column selectors
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
  
  # Calculate Cycle Lengths
  cycle_data <- reactive({
    req(user_data())
    req(input$id_col, input$date_col, input$menses_col, input$ovtoday_col)
    
    menstrualcycleR::calculate_mcyclength(
      data = user_data(),
      id = input$id_col,
      daterated = input$date_col,
      menses = input$menses_col,
      ovtoday = input$ovtoday_col
    )
  })
  
  output$cycle_data <- renderTable({
    req(cycle_data())
    head(cycle_data())
  })
  
  output$cycle_summary <- renderPrint({
    req(cycle_data())
    summary(cycle_data())
  })
  
  # Ovulation Analysis
  ovulation_summary <- reactive({
    req(cycle_data())
    menstrualcycleR::summary_ovulation(cycle_data())
  })
  
  output$ovulation_summary <- renderTable({
    req(ovulation_summary())
    ovulation_summary()$ovstatus_total
  })
  
  # Symptom Data Analysis
  symptom_analysis <- reactive({
    req(cycle_data())
    menstrualcycleR::cycledata_check(
      cycle_data(),
      symptom_columns = c("symptom")  # Can make dynamic
    )
  })
  
  output$symptom_analysis <- renderTable({
    req(symptom_analysis())
    symptom_analysis()$overall
  })
  
  # Cycle Plot
  output$cycle_plot <- renderPlot({
    req(cycle_data())
    result <- menstrualcycleR::cycle_plot(
      cycle_data(),
      symptom = "symptom",
      centering = "menses"
    )
    result$plot
  })
  
  # Download Processed Data
  output$download_results <- downloadHandler(
    filename = function() { paste("processed_cycle_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(cycle_data(), file, row.names = FALSE) }
  )
}
