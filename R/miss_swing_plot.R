miss_swing_plot <- function(data,
                   title="Probability of Miss"){
  data %>%
    setup_swing() %>%
    miss_gam_fit() %>%
    tile_plot(title)
}
