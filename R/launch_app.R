#' Launch the Menstrual Cycle Shiny App
#'
#' @export
launch_app <- function() {
  appDir <- system.file("shiny", package = "menstrualcycleR")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Try reinstalling `menstrualcycleR`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
