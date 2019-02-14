called_strike_plot <- function(data, title){
  data %>%
    setup_called() %>%
    strike_gam_fit() %>%
    tile_plot_p() +
    ggtitle(title) +
    centertitle()
}
