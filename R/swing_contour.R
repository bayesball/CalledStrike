swing_contour <- function(data,
                       L = seq(0, 1, by = 0.1),
                    title = "Probability of Swing"){
  data %>%
    setup_all() %>%
    swing_gam_fit() %>%
    contour_graph(L, title)
}
