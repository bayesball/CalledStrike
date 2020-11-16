called_strike_plot <- function(data,
              title = "Probability of Called Strike"){
  data %>%
    setup_called() %>%
    strike_gam_fit() %>%
    tile_plot(title)
}
