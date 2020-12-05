ShinyDemo <- function() {
  appDir <- system.file("shiny-examples", "ShinyDemo",
                        package = "CalledStrike")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `TeachBayes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
