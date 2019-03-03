swing_plot <- function(data, title=""){
  data %>%
    setup_all() %>%
    swing_gam_fit() %>%
    tile_plot_p() +
    ggtitle(title) +
    centertitle()
}
