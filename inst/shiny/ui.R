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
      
      #Date column with tooltip
      tags$div(
        style = "display: flex; align-items: center;",
        tags$label("Select Date Column:"),
        tags$i(
          class = "fas fa-info-circle",
          title = "This should be the date each symptom or entry was recorded. This needs to be in y-m-d format.",
          style = "margin-left: 5px; cursor: pointer;",
          `data-toggle` = "tooltip"
        )
      ),
      selectInput("date_col", NULL, choices = NULL),
      # menses column with tooltip
      tags$div(
        style = "display: flex; align-items: center;",
        tags$label("Select Menses Column:"),
        tags$i(
          class = "fas fa-info-circle",
          title = "This should be a binary column (0/1) where 1 = the first day of menses. This column needs to reflect the day of menses onset, and NOT each day of menstrual bleeding",
          style = "margin-left: 5px; cursor: pointer;",
          `data-toggle` = "tooltip"
        )
      ), 
      selectInput("menses_col", NULL, choices = NULL),
      # Ovulation column with tooltip
      tags$div(
        style = "display: flex; align-items: center;",
        tags$label("Select Ovulation Column:"),
        tags$i(
          class = "fas fa-info-circle",
          title = "This should be a binary column (0/1) where 1 = ovulation occurred that day. If You are using urinary LH-surge testing, the estimated day of ovulation is the day following the positive LH-test, and this needs to be reflected in the uploaded dataset.",
          style = "margin-left: 5px; cursor: pointer;",
          `data-toggle` = "tooltip"
        )
      ), 
      selectInput("ovtoday_col", NULL, choices = NULL),
      numericInput("lower_bound", "Lower Cycle Length Bound:", value = 21, min = 10, max = 50),
      numericInput("upper_bound", "Upper Cycle Length Bound:", value = 35, min = 10, max = 50),
      hr(),
      actionButton("process_data", "Process Data", class = "btn-success")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("View Data", tableOutput("data_preview")),
        tabPanel("Cycle Data Check",
                 sidebarLayout(
                   sidebarPanel(
                     checkboxGroupInput("cyclecheck_symptoms", "Select Symptoms to Check:", choices = NULL),
                     actionButton("run_cyclecheck", "Run Cycle Data Check", class = "btn-primary")
                   ),
                   mainPanel(
                     tags$h4("Symptom Coverage by ID"),
                     downloadButton("download_cyclecheck_by_id", "Download by ID Table"),
                     tableOutput("cyclecheck_by_id"),
                     
                     tags$h4("Overall Symptom Coverage"),
                     downloadButton("download_cyclecheck_overall", "Download Overall Table"),
                     tableOutput("cyclecheck_overall"),
                     
                     tags$h4("Symptom Data Plots"),
                     uiOutput("cyclecheck_plots")
                   )
                 )
        ),
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
        
        # Add this within your tabsetPanel() in the mainPanel section
        
        tabPanel("CPASS",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("cpass_id_select", "Select ID for CPASS Analysis:", choices = NULL),
                     hr(),
                     h4("Map Symptom Columns to DRSP Items"),
                     uiOutput("cpass_mapping_table"),
                     actionButton("run_cpass", "Run CPASS", class = "btn-primary")
                   ),
                   mainPanel(
                     uiOutput("cpass_plot_ui")
                   )
                 )
        ),
        
        tabPanel("Download Data", downloadButton("download_results", "Download Processed Data"))
      )
    )
  ),
  tags$script(HTML('
    $(function () {
      $(\'[data-toggle="tooltip"]\').tooltip();
    });
  '))
)
