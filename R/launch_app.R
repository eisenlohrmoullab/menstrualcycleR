#' Launch the Menstrual Cycle Shiny App
#'
#' This function launches an interactive Shiny application designed to help users upload and process their menstrual cycle data. 
#' The app provides tools to apply Phase-Aligned Cycle Time Scaling (PACTS), generate scaled cycleday variables, and visualize results 
#' in an intuitive, user-friendly interface.
#'
#' Users can upload a `.csv` file, process their data using built-in PACTS functionality, and explore cycle-aligned visualizations
#' to support analysis and interpretation.
#'
#' Requires the \pkg{shinyjs} and \pkg{cpass} packages, which are Suggests (not installed
#' automatically with menstrualcycleR) because they are only needed for this app, not for
#' \code{pacts_scaling()} or any other exported function. Install them with
#' \code{install.packages("shinyjs")} and \code{remotes::install_github("lasy/cpass")}.
#'
#' @export
launch_app <- function() {
  needed <- c(shinyjs = "install.packages(\"shinyjs\")",
              cpass = "remotes::install_github(\"lasy/cpass\")")
  missing <- needed[!vapply(names(needed), requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("launch_app() needs the following package(s), which are not installed: ",
         paste(names(missing), collapse = ", "), ". Install with: ",
         paste(missing, collapse = "; then "), ".", call. = FALSE)
  }
  appDir <- system.file("shiny", package = "menstrualcycleR")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Try reinstalling `menstrualcycleR`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
