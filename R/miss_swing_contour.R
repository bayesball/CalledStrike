miss_swing_contour <- function(data,
                            L = seq(0, 1, by = 0.1),
                  title = "Probability of Miss"){
  data %>%
    setup_swing() %>%
    miss_gam_fit() %>%
    contour_graph(L, title)
}

