library(shiny)
library(ggplot2)
library(dplyr)
library(CalledStrike)

ui <- fluidPage(
  fluidRow(
    h2(id="big-heading", " Pitch Value Across Counts"),
    tags$style(HTML("#big-heading{color: blue;}")),
    column(4, wellPanel(
      fileInput("file1", "Read in Statcast CSV File",
                accept = ".csv"),
      checkboxInput("header", "Header", TRUE),
      radioButtons("p_type", "Pitch Type:",
                   c("CH", "CU", "FC", "FF", "FT",
                     "SI", "SL"),
                   inline = TRUE),
      radioButtons("p_side", "Pitcher Side:",
                   c("L", "R"),
                   inline = TRUE),
      radioButtons("b_side", "Batter Side:",
             c("L", "R"),
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

  options(shiny.maxRequestSize=60*1024^2)

  the_data <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    read.csv(file$datapath, header = input$header)
  })

  data <- eventReactive(input$goButton, {
    df <- the_data()
    df <- filter(df,
                 stand == input$b_side,
                 p_throws == input$p_side,
                 pitch_type == input$p_type,
                 Count %in% input$counts) %>%
        mutate(The_Count = paste("Count =", Count))
    df2 <- split(df, df$The_Count)
    N <- length(input$counts)
    NCOL <- ifelse(N <= 4, 2, 3)
    the_title <- paste("Pitch Type: ", input$p_type,
                      ", Pitcher: ", input$p_side,
                      ", Batter: ", input$b_side,
                      sep = "")
    pitch_value_contour(df2,
                        title = the_title,
                        NCOL = NCOL)
  })
  output$mplot <- renderPlot({
    data()
  })
}


shinyApp(ui, server)
