ls_plot <- function(data,
                    title = "Launch Velocity"){
  data %>%
    setup_inplay() %>%
    ls_gam_fit() %>%
    tile_plot(title)
}
