#' Launch the Menstrual Cycle Shiny App
#'
#' This function launches an interactive Shiny application designed to help users upload and process their menstrual cycle data. 
#' The app provides tools to apply Phase-Aligned Cycle Time Scaling (PACTS), generate scaled cycleday variables, and visualize results 
#' in a browser interface.
#'
#' Users can upload a `.csv` file, process their data using built-in PACTS functionality, and explore cycle-aligned visualizations
#' to support analysis and interpretation.
#'
#' Requires three packages that are not installed automatically with menstrualcycleR,
#' because they are needed only for this app and not for \code{pacts_scaling()} or any
#' other exported function. \pkg{shinyjs} and \pkg{writexl} are suggested dependencies and
#' install from CRAN; \pkg{writexl} backs the app's download buttons. \pkg{cpass} is not on CRAN and installs
#' from GitHub with \code{remotes::install_github("lasy/cpass")}; it powers the app's
#' optional CPASS tab only. \code{launch_app()} checks for both and reports which are
#' missing rather than failing part-way through.
#'
#' @return Called for its side effect of launching the Shiny application. Returns the
#'   result of \code{shiny::runApp()} invisibly; the call blocks until the app is closed.
#'   Throws an error, without launching, if any required package is missing or if the app
#'   directory cannot be found.
#' @export
launch_app <- function() {
  needed <- c(shinyjs = "install.packages(\"shinyjs\")",
              writexl = "install.packages(\"writexl\")",
              cpass  = "remotes::install_github(\"lasy/cpass\")")
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
