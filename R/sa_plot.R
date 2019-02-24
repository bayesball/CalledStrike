sa_plot <- function(data, title = ""){
  data %>%
    setup_inplay() %>%
    sa_gam_fit() %>%
    tile_plot_m() +
    ggtitle(title) +
    centertitle()
}
