sa_plot <- function(data,
                    title = "Spray Angle"){
  data %>%
    setup_inplay() %>%
    sa_gam_fit() %>%
    tile_plot(title)
}
