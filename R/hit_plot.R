hit_plot <- function(data, title){
  data %>%
    setup_inplay() %>%
    hr_h_gam_fit(HR = FALSE) %>%
    tile_plot_p() +
    ggtitle(title) +
    centertitle()
}
