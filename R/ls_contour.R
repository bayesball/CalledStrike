ls_contour <- function(data, L = seq(75, 105, 5),
                       title = "Launch Velocity"){
  data %>%
    setup_inplay() %>%
    ls_gam_fit()  %>%
    contour_graph(L, title)
}
