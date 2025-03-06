library(shiny)

ui <- fluidPage(
  titlePanel("Menstrual Cycle Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV Data:", accept = ".csv"),
      actionButton("load_data", "Load Data", class = "btn-primary"),
      hr(),
      selectInput("id_col", "Select ID Column:", choices = NULL),
      selectInput("date_col", "Select Date Column:", choices = NULL),
      selectInput("menses_col", "Select Menses Column:", choices = NULL),
      selectInput("ovtoday_col", "Select Ovulation Column:", choices = NULL)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("View Data", tableOutput("data_preview")),
        tabPanel("Cycle Lengths", verbatimTextOutput("cycle_summary"), tableOutput("cycle_data")),
        tabPanel("Ovulation Analysis", tableOutput("ovulation_summary")),
        tabPanel("Symptom Data", tableOutput("symptom_analysis")),
        tabPanel("Cycle Plot", plotOutput("cycle_plot")),
        tabPanel("Download", downloadButton("download_results", "Download Processed Data"))
      )
    )
  )
)
