library(shiny)
library(CalledStrike)
library(dplyr)
library(readr)
library(ggplot2)

# Define UI ----
ui <- fluidPage(
  # Application title
  h1(id="big-heading", "Batting Measure"),
  tags$style(HTML("#big-heading{color: blue;}")),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Read in Statcast CSV File",
                accept = ".csv"),
      checkboxInput("header", "Header", TRUE),
      textInput("name", "Batter Name:", value = ""),
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
  options(shiny.maxRequestSize = 60 * 1024 ^ 2)
  get_id <- function(name){
    names <- unlist(strsplit(name, " "))
    chadwick %>%
      filter(name_last == names[2],
             name_first == names[1]) %>%
      top_n(-1) %>%
      pull(key_mlbam)
  }
  the_data <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    read.csv(file$datapath, header = input$header)
  })
  data <- eventReactive(input$goButton, {
      df <- the_data()
      df1 <- filter(df,  player_name == input$name) %>%
        mutate(PitchType = ifelse(pitch_type %in%
                c("FC", "FF", "FO", "FS", "FT"),
                  "Fastball", "Offspeed"))
      dpt <- split(df1, df1$PitchType)
      dpt1 <- split(dpt[[1]], dpt[[1]]$p_throws)
      dpt2 <- split(dpt[[2]], dpt[[2]]$p_throws)
      d <- c(dpt1, dpt2)
      names(d) <- c("1 Pitch Left - Fastball",
                    "2 Pitch Right - Fastball",
                    "3 Pitch Left - Off-Speed",
                    "4 Pitch Right - Off-Speed")
    if(input$graphtype == "contour"){
      if(input$measure == "called strike"){
        dplot <- called_strike_contour(d,
                   L = seq(0.1, 0.9,
                           by = 0.1 * exp(input$dL)),
                   NCOL = 2)
      } else if (input$measure == "miss"){
        dplot <- miss_swing_contour(d,
                     L = seq(0, 1,
                        by = 0.05 * exp(input$dL)),
                     NCOL = 2)
      } else if (input$measure == "batting average"){
        dplot <- hit_contour(d,
                    L = seq(0, 1,
                        by = 0.05 * exp(input$dL)),
                    NCOL = 2)
      } else if (input$measure == "wOBA"){
        dplot <- woba_contour(d,
                    L = seq(0, 1,
                        by = 0.05 * exp(input$dL)),
                    NCOL = 2)
      } else if (input$measure == "home run"){
        dplot <- home_run_contour(d,
                    L = seq(0, 1,
                        by = 0.02 * exp(input$dL)),
                    NCOL = 2)
      } else if (input$measure == "swing"){
        dplot <- swing_contour(d,
                      L = seq(0, 1,
                           by = 0.05 * exp(input$dL)),
                      NCOL = 2)
      } else if (input$measure == "contact"){
        dplot <- contact_swing_contour(d,
                      L = seq(0, 1,
                          by = 0.05 * exp(input$dL)),
                      NCOL = 2)
      } else if (input$measure == "in-play"){
        dplot <- inplay_swing_contour(d,
                      L = seq(0, 1,
                          by = 0.05 * exp(input$dL)),
                      NCOL = 2)
      } else if (input$measure == "launch speed"){
        dplot <- ls_contour(d,
                   L = seq(50, 110,
                           by = 5 * exp(input$dL)),
                   NCOL = 2)
      } else if (input$measure == "launch angle"){
        dplot <- la_contour(d,
                   L = seq(-10, 40,
                           by = 10 * exp(input$dL)),
                   NCOL = 2)
      } else if (input$measure == "spray angle"){
        dplot <- sa_contour(d,
                   L = seq(-40, 40,
                           by = 10 * exp(input$dL)),
                   NCOL = 2)
      } else if (input$measure == "expected wOBA"){
        dplot <- ewoba_contour(d,
                      L = seq(0, 1,
                      by = 0.05 * exp(input$dL)),
                      NCOL = 2)
      } else if (input$measure == "expected BA"){
        dplot <- ehit_contour(d,
                     L = seq(0, 1,
                    by = 0.05 * exp(input$dL)),
                    NCOL = 2)
      }} else if(input$graphtype == "tile"){
        if(input$measure == "called strike"){
          dplot <- called_strike_plot(d,
                                      NCOL = 2)
        } else if (input$measure == "swing"){
          dplot <- swing_plot(d,
                                      NCOL = 2)
        } else if (input$measure == "contact"){
          dplot <- contact_swing_plot(d,
                                      NCOL = 2)
        } else if (input$measure == "miss"){
          dplot <- miss_swing_plot(d,
                                   NCOL = 2)
        } else if (input$measure == "batting average"){
          dplot <- hit_plot(d,
                            NCOL = 2)
        } else if (input$measure == "home run"){
          dplot <- home_run_plot(d,
                                 NCOL = 2)
        } else if (input$measure == "wOBA"){
          dplot <- woba_plot(d,
                             NCOL = 2)
        } else if (input$measure == "in-play"){
          dplot <- inplay_swing_plot(d,
                                     NCOL = 2)
        } else if (input$measure == "launch speed"){
          dplot <- ls_plot(d,
                           NCOL = 2)
        } else if (input$measure == "launch angle"){
          dplot <- la_plot(d,
                           NCOL = 2)
        } else if (input$measure == "spray angle"){
          dplot <- sa_plot(d,
                           NCOL = 2)
        } else if (input$measure == "expected wOBA"){
          dplot <- ewoba_plot(d,
                              NCOL = 2)
        } else if (input$measure == "expected BA"){
          dplot <-ehit_plot(d,
                            NCOL = 2)
        }}
      dplot +
        labs(subtitle = input$name) +
        theme(
          plot.subtitle =
            element_text(hjust = 0.5,
                         color = "blue")
        )
   })
  output$distPlot <- renderPlot({
    data()
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
