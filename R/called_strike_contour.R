called_strike_contour <- function(data,
                                  L = c(0.5, 0.9),
              title = "Probability of Called Strike"){
  data %>%
    setup_called() %>%
    strike_gam_fit() %>%
    contour_graph(L, title)
}


