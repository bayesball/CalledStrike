PitchLocation2 <- function() {
  appDir <- system.file("shiny-examples", "PitchLocation2",
                        package = "CalledStrike")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `TeachBayes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
