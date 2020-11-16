la_plot <- function(data,
                    title = "Launch Angle"){
  data %>%
    setup_inplay() %>%
    la_gam_fit() %>%
    tile_plot(title)
}
