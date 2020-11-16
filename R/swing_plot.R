swing_plot <- function(data,
                       title="Probability of Swing"){
  data %>%
    setup_all() %>%
    swing_gam_fit() %>%
    tile_plot(title)
}
