miss_swing_plot <- function(data, title=""){
  data %>%
    setup_swing() %>%
    miss_gam_fit() %>%
    tile_plot_p() +
    ggtitle(title) +
    centertitle()
}
