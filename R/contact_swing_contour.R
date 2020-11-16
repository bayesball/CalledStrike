contact_swing_contour <- function(data,
                            L = seq(0, 1, by = 0.1),
          title = "Probability of Making Contact"){
  data %>%
    setup_swing() %>%
    contact_gam_fit() %>%
    contour_graph(L, title)
}

