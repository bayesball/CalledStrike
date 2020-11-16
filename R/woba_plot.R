woba_plot <- function(data,
                    title = "Average wOBA"){
  data %>%
    setup_inplay() %>%
    woba_gam_fit() %>%
    tile_plot(title)
}
