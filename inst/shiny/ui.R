ui <- fluidPage(
  titlePanel("Phase Aligned Cycle Time Scaling (PACTS) with menstrualcycleR"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV Data:", accept = ".csv"),
      actionButton("load_data", "Load Data", class = "btn-primary"),
      hr(),
      numericInput("lower_bound", "Lower Bound of Cycle Length:", 21, min = 1),
      numericInput("upper_bound", "Upper Bound of Cycle Length:", 35, min = 1),
      selectInput("id_col", "Select ID Column:", choices = NULL),
      selectInput("date_col", "Select Date Column:", choices = NULL),
      selectInput("menses_col", "Select Menses Column:", choices = NULL),
      selectInput("ovtoday_col", "Select Ovulation Column:", choices = NULL),
      actionButton("process_data", "Process Data", class = "btn-success")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("View Data", tableOutput("data_preview")),
        tabPanel("Cycle Plot",
                 selectInput("symptom_col_plot", "Select Symptom:", choices = NULL),
                 selectInput("plot_centering", "Centering:", choices = c("menses", "ovulation")),
                 checkboxInput("plot_impute", "Include Imputed Data", TRUE),
                 numericInput("rollingavg", "Rolling Day Average:", 5, min = 1),
                 actionButton("update_plot", "Update Plot"),
                 plotOutput("cycle_plot"),
                 downloadButton("download_plot", "Download Cycle Plot")
        ),
        tabPanel("Individual Cycle Plots", uiOutput("individual_cycle_plots"))
      )
    )
  )
)
