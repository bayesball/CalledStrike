sa_contour <- function(data,
                       L = seq(-40, 40, by = 10),
                  title = "Spray Angle"){
  data %>%
    setup_inplay() %>%
    sa_gam_fit() %>%
    contour_graph(L, title)
}

