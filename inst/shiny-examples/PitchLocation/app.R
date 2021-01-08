library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(baseballr)

#pitchers <- read_csv("2019pitchers.csv")
#sc <- read_csv("sc2019_20.csv")

ui <- fluidPage(
  fluidRow(
    column(4, wellPanel(
     selectInput("name", "Select 2019 Pitcher:",
                  pitchers_2019$Name),
      radioButtons("side", "Batter Side:",
             c("L", "R"),
             inline = TRUE),
      radioButtons("ptype", "Pitch Type:",
             c("Fastball", "Offspeed"),
             inline = TRUE),
      checkboxGroupInput(
            "counts",
            "Choose Counts to Compare:",
            choices = c("0-0" = "0-0",
                        "0-1" = "0-1",
                        "1-0" = "1-0",
                        "0-2" = "0-2",
                        "1-1" = "1-1",
                        "2-0" = "2-0",
                        "1-2" = "1-2",
                        "2-1" = "2-1",
                        "3-0" = "3-0",
                        "2-2" = "2-2",
                        "3-1" = "3-1",
                        "3-2" = "3-2"),
            selected = "0-0",
            inline = TRUE
),
      tags$head(
        tags$style(HTML('#goButton{background-color:orange}'))
      ),
      actionButton("goButton", "Make Plot")
    )),
    column(8,
           plotOutput("mplot", height = "500px")
           )
  )
)

server <- function(input, output, session) {

  options(shiny.maxRequestSize=30*1024^2)
  get_id <- function(name){
    names <- unlist(strsplit(name, " "))
    playerid_lookup(last_name = names[2],
                    first_name = names[1]) %>%
      dplyr::select(mlbam_id) %>% top_n(-1) %>% pull()
  }
  add_zone <- function(Color = "red"){
    topKzone <- 3.5
    botKzone <- 1.6
    inKzone <- -0.85
    outKzone <- 0.85
    kZone <- data.frame(
      x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
      y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
    )
    geom_path(aes(.data$x, .data$y), data=kZone,
              lwd=1, col=Color)
  }
  filled_contour_compare <- function(d,
                                     Pitcher = pitch_id,
                                     name = "",
                                     side = "R",
                                     ptype = "Fastball",
                                     count_v = cts){

    d %>%
      mutate(PitchType = ifelse(pitch_type %in%
                           c("FC", "FF", "FO", "FS", "FT"),
                                "Fastball", "Offspeed"),
             Count = paste(balls, strikes, sep="-"),
             TheCount = paste("Count =", Count)) -> d
    d <- filter(d,
                stand == side,
                PitchType == ptype,
                pitcher == Pitcher,
                Count %in% count_v)

    title <- paste(name, ":", side, ptype)

    ggplot(d, aes(plate_x, plate_z)) +
      geom_density_2d_filled(contour_var = "ndensity") +
      add_zone() +
      xlim(-2.5, 2.5) +
      ylim(0, 5) +
      theme(legend.position = "none") +
      facet_wrap(~ TheCount, ncol = 2) +
      theme(text=element_text(size=18)) +
      ggtitle(title) +
      coord_equal() +
      theme(plot.title = element_text(colour = "red",
                                      size = 24,
                                      hjust = 0.5, vjust = 0.8, angle = 0))
  }

  data <- eventReactive(input$goButton, {
    pid <- pitchers_2019 %>%
      filter(Name == input$name) %>%
      select(SC_id) %>% pull()

    filled_contour_compare(sc_pitchers_2019,
                           pid,
                           input$name,
                           input$side,
                           input$ptype,
                           input$counts)
  })
  output$mplot <- renderPlot({
    data()
  })
}



shinyApp(ui, server)
