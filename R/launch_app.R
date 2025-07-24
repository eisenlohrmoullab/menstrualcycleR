#' Launch the Menstrual Cycle Shiny App
#'
#' This function launches an interactive Shiny application designed to help users upload and process their menstrual cycle data. 
#' The app provides tools to apply Phase-Aligned Cycle Time Scaling (PACTS), generate scaled cycleday variables, and visualize results 
#' in an intuitive, user-friendly interface.
#'
#' Users can upload a `.csv` file, process their data using built-in PACTS functionality, and explore cycle-aligned visualizations 
#' to support analysis and interpretation.

#'
#' @export
launch_app <- function() {
  appDir <- system.file("shiny", package = "menstrualcycleR")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Try reinstalling `menstrualcycleR`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
