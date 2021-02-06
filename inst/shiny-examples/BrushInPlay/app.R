library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
      h2("Brushing In-Play Batting Averages"),
      textInput("name", "Batter Name:", value = ""),
      plotOutput("plot", brush = "plot_brush"),
      tableOutput("data")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    add_zone <- function(){
      topKzone <- 3.5
      botKzone <- 1.6
      inKzone <- -0.85
      outKzone <- 0.85
      kZone <- data.frame(
        x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
        y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
      )
      geom_path(aes(.data$x, .data$y),
                data=kZone, lwd = 1)
    }
    ggplot() +
      geom_point(data = filter(sc2019_ip,
                    player_name == input$name),
                 aes(plate_x, plate_z, color = H)) +
      add_zone() +
      ggtitle(input$name) +
      coord_equal()
  }, res = 96)

  output$data <- renderTable({
    req(input$plot_brush)
    sc1 <- brushedPoints(filter(sc2019_ip,
                    player_name == input$name),
                    input$plot_brush)
    data.frame(Name = input$name,
               xlo = min(sc1$plate_x),
               xhi = max(sc1$plate_x),
               zlo = min(sc1$plate_z),
               zhi = max(sc1$plate_z),
               AB = nrow(sc1),
               H = sum(sc1$H),
               BABIP = sum(sc1$H) / nrow(sc1))
  }, digits = 3, width = '75%')
}

shinyApp(ui = ui, server = server)
