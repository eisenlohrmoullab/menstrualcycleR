library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
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
      numericInput("lower_bound", "Lower Cycle Length Bound:", value = 21, min = 10, max = 50),
      numericInput("upper_bound", "Upper Cycle Length Bound:", value = 35, min = 10, max = 50),
      hr(),
      actionButton("process_data", "Process Data", class = "btn-success")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("View Data", tableOutput("data_preview")),
        tabPanel("Processed Data", tableOutput("cycle_data")),
        tabPanel("Cycle Summary", verbatimTextOutput("cycle_summary")),
        tabPanel("Ovulation Analysis",
                 tableOutput("ovulation_summary"),
                 tableOutput("ovulation_summary_id")),
        
        tabPanel("Cycle Plot",
                 sidebarPanel(
                   selectInput("symptom_col_plot", "Select Symptom for Plot:", choices = NULL),
                   selectInput("plot_centering", "Centering:", choices = c("menses", "ovulation")),
                   checkboxInput("plot_impute", "Include Imputed Data", value = TRUE),
                   selectInput("plot_y_scale", "Y-Axis Scale:",
                               choices = c("person-centered", "person-centered_roll", "means")),
                   numericInput("rollingavg", "Rolling Day Average:", value = 5, min = 1, max = 10),
                   actionButton("update_plot", "Update Plot", class = "btn-primary"),
                   hr(),
                   downloadButton("download_plot", "Download Cycle Plot")
                 ),
                 mainPanel(
                   plotOutput("cycle_plot")
                 )
        ),
        
        tabPanel("Individual Cycle Plot Viewer",
                 sidebarPanel(
                   selectInput("selected_id", "Select ID", choices = NULL),
                   checkboxGroupInput("selected_symptoms", "Select Symptoms", choices = NULL),
                   selectInput("centering_mode", "Centering", choices = c("menses", "ovulation")),
                   selectInput("y_scale_mode", "Y-axis Scale", choices = c("person-centered", "person-centered_roll", "raw", "roll")),
                   numericInput("rollingavg_input", "Rolling Day Average", value = 5, min = 1),
                   checkboxInput("include_impute_toggle", "Include Imputed Data", value = TRUE),
                   actionButton("run_individual_plot", "Generate Plots", class = "btn-primary")
                 ),
                 mainPanel(
                   uiOutput("individual_plot_output")
                 )
        ),
        
        tabPanel("CPASS",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("cpass_id_select", "Select ID:", choices = NULL),
                     helpText("Type a number (1–24) next to each symptom column to map it to a DRSP item number."),
                     uiOutput("cpass_mapping_table"),
                     actionButton("run_cpass", "Run CPASS", class = "btn-primary")
                   ),
                   mainPanel(
                     plotOutput("cpass_plot")
                   )
                 )
        ),
        tabPanel("Download Data", downloadButton("download_results", "Download Processed Data"))
      )
    )
  )
)
