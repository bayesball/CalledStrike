hit_plot <- function(data,
                     title = "Probability of a Hit"){
  data %>%
    setup_inplay() %>%
    hr_h_gam_fit(HR = FALSE) %>%
    tile_plot(title)
}
