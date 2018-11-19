# runPcclustApp.R

#' \code{runPcclustApp} launch the shiny app distributed with this package framework
#'
#' \code{runPcclustApp} launches the shiny app for which the code has been placed in  \code{./inst/shiny-scripts/rptApp/}.
#' @export

runPcclustApp <- function() {
  appDir <- system.file("shiny-scripts", "pcclustApp", package = "pcclust")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}

# [END]
