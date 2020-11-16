la_contour <- function(data, L = seq(-10, 40, 10),
                       title = "Launch Angle"){
  data %>%
    setup_inplay() %>%
    la_gam_fit() %>%
    contour_graph(L, title)
}
