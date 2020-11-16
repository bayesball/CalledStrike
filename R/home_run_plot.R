home_run_plot <- function(data,
            title = "Probability of Home Run"){
  data %>%
    setup_inplay() %>%
    hr_h_gam_fit() %>%
    tile_plot(title)
}
