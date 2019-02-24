ls_plot <- function(data, title = ""){
  data %>%
    setup_inplay() %>%
    ls_gam_fit() %>%
    tile_plot_m() +
    ggtitle(title) +
    centertitle()
}
