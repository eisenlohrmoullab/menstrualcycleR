library(shiny)

ui <- fluidPage(
  titlePanel("Phase Aligned Cycle Time Scaling (PACTS) with menstrualcycleR"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV Data:", accept = ".csv"),
      actionButton("load_data", "Load Data", class = "btn-primary"),
      hr(),
      selectInput("id_col", "Select ID Column:", choices = NULL),
      selectInput("date_col", "Select Date Column:", choices = NULL),
      selectInput("menses_col", "Select Menses Column:", choices = NULL),
      selectInput("ovtoday_col", "Select Ovulation Column:", choices = NULL),
      hr(),
      actionButton("process_data", "Process Data", class = "btn-success")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("View Data", tableOutput("data_preview")),
        tabPanel("Data Availability", verbatimTextOutput("cycle_summary"), tableOutput("cycle_data")),
        tabPanel("Ovulation Analysis", tableOutput("ovulation_summary")),
        
        # Symptom Analysis Tab
        tabPanel("Symptom Data",
                 sidebarPanel(
                   selectInput("symptom_col_analysis", "Select Symptom Column for Analysis:", choices = NULL)
                 ),
                 mainPanel(
                   tableOutput("symptom_analysis")
                 )
        ),
        
        # Cycle Plot Tab with separate symptom selection
        tabPanel("Cycle Plot",
                 sidebarPanel(
                   selectInput("symptom_col_plot", "Select Symptom Column for Plot:", choices = NULL),
                   selectInput("plot_centering", "Centering Phase:", choices = c("menses", "ovulation")),
                   checkboxInput("plot_impute", "Include Imputed Data", value = TRUE),
                   actionButton("update_plot", "Update Plot", class = "btn-primary"),
                   hr(),
                   downloadButton("download_plot", "Download Cycle Plot")
                 ),
                 mainPanel(
                   plotOutput("cycle_plot")
                 )
        ),
        
        tabPanel("Download", downloadButton("download_results", "Download Processed Data"))
      )
    )
  )
)
