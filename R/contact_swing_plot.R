contact_swing_plot <- function(data,
                   title="Probability of Contact"){
  data %>%
    setup_swing() %>%
    contact_gam_fit() %>%
    tile_plot(title)
}
