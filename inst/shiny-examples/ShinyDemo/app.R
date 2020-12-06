library(shiny)
library(CalledStrike)
library(dplyr)
library(readr)

# Define UI ----
ui <- fluidPage(
  # Application title
  h1(id="big-heading", "CalledStrike Demo - 2019 Season"),
  tags$style(HTML("#big-heading{color: blue;}")),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("players",
                "Choose Players:",
                choices =
      unique(as.character(sc_sample$player_name))[-10],
      selected = unique(as.character(
        sc_sample$player_name))[1:4]
      ),
      selectInput("measure",
                  "Select Measure:",
                  c("called strike",
                    "swing",
                    "miss",
                    "contact",
                    "in-play",
                    "launch speed",
                    "launch angle",
                    "spray angle",
                    "home run",
                    "batting average",
                    "wOBA",
                    "expected BA",
                    "expected wOBA"
                    )
                  ),
      radioButtons("graphtype",
                  "Select Graph Type:",
                  c("contour",
                    "tile")
      ),
      sliderInput("dL", "Choose Contour Parameter:",
                   0.0, min = -1.5, max = 1.5),
      tags$head(
        tags$style(HTML('#goButton{background-color:orange}'))
      ),
      actionButton("goButton", "UPDATE PLOT")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", height="550px")
    )
  ))

# Define server logic ----
server <- function(input, output) {

  data <- eventReactive(input$goButton, {
      sc_sample$Player <-
          as.character(sc_sample$player_name)
      sc <- filter(sc_sample,
                   Player %in% input$players)
      d <- split(sc,  sc$Player)
      n <- function(x){
        if(x <= 4){
          2
        } else {
          3
        }
      }
    if(input$graphtype == "contour"){
      if(input$measure == "called strike"){
        dplot <- called_strike_contour(d,
                   L = seq(0.1, 0.9,
                           by = 0.1 * exp(input$dL)),
                   NCOL = n(length(input$players)))
      } else if (input$measure == "miss"){
        dplot <- miss_swing_contour(d,
                     L = seq(0, 1,
                        by = 0.05 * exp(input$dL)),
                     NCOL = n(length(input$players)))
      } else if (input$measure == "batting average"){
        dplot <- hit_contour(d,
                    L = seq(0, 1,
                        by = 0.05 * exp(input$dL)),
                    NCOL = n(length(input$players)))
      } else if (input$measure == "wOBA"){
        dplot <- woba_contour(d,
                    L = seq(0, 1,
                        by = 0.05 * exp(input$dL)),
                    NCOL = n(length(input$players)))
      } else if (input$measure == "home run"){
        dplot <- home_run_contour(d,
                    L = seq(0, 1,
                        by = 0.02 * exp(input$dL)),
                    NCOL = n(length(input$players)))
      } else if (input$measure == "swing"){
        dplot <- swing_contour(d,
                      L = seq(0, 1,
                           by = 0.05 * exp(input$dL)),
                      NCOL = n(length(input$players)))
      } else if (input$measure == "contact"){
        dplot <- contact_swing_contour(d,
                      L = seq(0, 1,
                          by = 0.05 * exp(input$dL)),
                      NCOL = n(length(input$players)))
      } else if (input$measure == "in-play"){
        dplot <- inplay_swing_contour(d,
                      L = seq(0, 1,
                          by = 0.05 * exp(input$dL)),
                      NCOL = n(length(input$players)))
      } else if (input$measure == "launch speed"){
        dplot <- ls_contour(d,
                   L = seq(50, 110,
                           by = 5 * exp(input$dL)),
                   NCOL = n(length(input$players)))
      } else if (input$measure == "launch angle"){
        dplot <- la_contour(d,
                   L = seq(-10, 40,
                           by = 10 * exp(input$dL)),
                   NCOL = n(length(input$players)))
      } else if (input$measure == "spray angle"){
        dplot <- sa_contour(d,
                   L = seq(-40, 40,
                           by = 10 * exp(input$dL)),
                   NCOL = n(length(input$players)))
      } else if (input$measure == "expected wOBA"){
        dplot <- ewoba_contour(d,
                      L = seq(0, 1,
                      by = 0.05 * exp(input$dL)),
                      NCOL = n(length(input$players)))
      } else if (input$measure == "expected BA"){
        dplot <- ehit_contour(d,
                     L = seq(0, 1,
                    by = 0.05 * exp(input$dL)),
                    NCOL = n(length(input$players)))
      }} else if(input$graphtype == "tile"){
        if(input$measure == "called strike"){
          dplot <- called_strike_plot(d,
                    NCOL = n(length(input$players)))
        } else if (input$measure == "swing"){
          dplot <- swing_plot(d,
                     NCOL = n(length(input$players)))
        } else if (input$measure == "contact"){
          dplot <- contact_swing_plot(d,
                    NCOL = n(length(input$players)))
        } else if (input$measure == "miss"){
          dplot <- miss_swing_plot(d,
                    NCOL = n(length(input$players)))
        } else if (input$measure == "batting average"){
          dplot <- hit_plot(d,
                   NCOL = n(length(input$players)))
        } else if (input$measure == "home run"){
          dplot <- home_run_plot(d,
                  NCOL = n(length(input$players)))
        } else if (input$measure == "wOBA"){
          dplot <- woba_plot(d,
                  NCOL = n(length(input$players)))
        } else if (input$measure == "in-play"){
          dplot <- inplay_swing_plot(d,
                  NCOL = n(length(input$players)))
        } else if (input$measure == "launch speed"){
          dplot <- ls_plot(d,
                  NCOL = n(length(input$players)))
        } else if (input$measure == "launch angle"){
          dplot <- la_plot(d,
                  NCOL = n(length(input$players)))
        } else if (input$measure == "spray angle"){
          dplot <- sa_plot(d,
                  NCOL = n(length(input$players)))
        } else if (input$measure == "expected wOBA"){
          dplot <- ewoba_plot(d,
                     NCOL = n(length(input$players)))
        } else if (input$measure == "expected BA"){
          dplot <-ehit_plot(d,
                    NCOL = n(length(input$players)))
        }}
      dplot
   })
  output$distPlot <- renderPlot({
    data()
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
