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
        tabPanel("Cycle Plot",
                 selectInput("symptom_col_plot", "Select Symptom:", choices = NULL),
                 selectInput("plot_centering", "Centering:", choices = c("menses", "ovulation")),
                 selectInput("plot_y_scale", "Y Scale:", choices = c("person-centered", "person-centered_roll", "means")),
                 checkboxInput("plot_impute", "Include Imputed Data", TRUE),
                 numericInput("rollingavg", "Rolling Day Average:", 5, min = 1),
                 actionButton("update_plot", "Update Plot"),
                 plotOutput("cycle_plot"),
                 downloadButton("download_plot", "Download Cycle Plot")
        ),
        
        tabPanel("Individual Cycle Plots",
                 selectInput("id_selected", "Select Participant ID:", choices = NULL),
                 checkboxGroupInput("symptom_cols_individual", "Select Symptoms:", choices = NULL),
                 selectInput("plot_centering_individual", "Centering:", choices = c("menses", "ovulation")),
                 selectInput("individual_y_scale", "Y Scale:", choices = c("person-centered", "person-centered_roll", "raw", "roll")),
                 checkboxInput("plot_impute_individual", "Include Imputed Data", TRUE),
                 numericInput("individual_rollingavg", "Rolling Day Average:", 5, min = 1),
                 actionButton("update_individual_plot", "Update Plot"),
                 uiOutput("individual_cycle_plots")
        )
      )
    )
  )
)
