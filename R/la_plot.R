la_plot <- function(data, title){
  data %>%
    setup_inplay() %>%
    la_gam_fit() %>%
    tile_plot_m() +
    ggtitle(title) +
    centertitle()
}
