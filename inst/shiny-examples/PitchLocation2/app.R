library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  fluidRow(
    column(4, wellPanel(
      fileInput("file1", "Read in Statcast CSV File",
                accept = ".csv"),
      checkboxInput("header", "Header", TRUE),
      textInput("name", "Pitcher Name:", value = ""),
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
           plotOutput("mplot", height = "600px")
           )
  )
)

server <- function(input, output, session) {

  options(shiny.maxRequestSize=30*1024^2)
  get_id <- function(name){
    names <- unlist(strsplit(name, " "))
    chadwick %>%
    filter(name_last == names[2],
           name_first == names[1]) %>%
      dplyr::select(key_mlbam) %>%
      top_n(-1) %>% pull()
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
                                     count_v = cts,
                                     NCOL = 2){

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
      facet_wrap(~ TheCount, ncol = NCOL) +
      theme(text=element_text(size=18)) +
      ggtitle(title) +
      coord_equal() +
      theme(plot.title = element_text(colour = "red",
                                      size = 24,
                                      hjust = 0.5, vjust = 0.8, angle = 0))
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
    N <- length(input$counts)
    NCOL <- ifelse(N <= 4, 2, 3)
    pid <- get_id(input$name)
    filled_contour_compare(df,
                           pid,
                           input$name,
                           input$side,
                           input$ptype,
                           input$counts,
                           NCOL)
  })
  output$mplot <- renderPlot({
    data()
  })
}



shinyApp(ui, server)
