ui <- fluidPage(
  titlePanel("Phase Aligned Cycle Time Scaling (PACTS) with menstrualcycleR"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV Data:", accept = ".csv"),
      actionButton("load_data", "Load Data", class = "btn-primary"),
      hr(),
      
      numericInput("lower_bound", "Lower Bound of Cycle Length:", value = 21, min = 10, max = 40),
      numericInput("upper_bound", "Upper Bound of Cycle Length:", value = 35, min = 10, max = 40),
      
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
        tabPanel("Ovulation Analysis", tableOutput("ovulation_summary")),
        
        tabPanel("Cycle Plot",
                 sidebarPanel(
                   selectInput("symptom_col_plot", "Select Symptom:", choices = NULL),
                   selectInput("plot_centering", "Centering:", choices = c("menses", "ovulation")),
                   checkboxInput("plot_impute", "Include Imputed Data", value = TRUE),
                   actionButton("update_plot", "Update Plot"),
                   downloadButton("download_plot", "Download Cycle Plot")
                 ),
                 mainPanel(plotOutput("cycle_plot"))
        ),
        
        tabPanel("Individual Cycle Plot",
                 sidebarPanel(
                   selectInput("id_selected", "Select ID:", choices = NULL),
                   checkboxGroupInput("symptom_cols_individual", "Select Symptoms:", choices = NULL)
                 ),
                 mainPanel(uiOutput("individual_cycle_plots"))
        ),
        
        tabPanel("Download", downloadButton("download_results", "Download Processed Data"))
      )
    )
  )
)
