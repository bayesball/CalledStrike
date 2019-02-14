home_run_plot <- function(data, title){
  data %>%
    setup_inplay() %>%
    hr_h_gam_fit() %>%
    tile_plot_p() +
    ggtitle(title) +
    centertitle()
}
