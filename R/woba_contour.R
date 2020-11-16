woba_contour <- function(data, L = seq(75, 105, 5),
                       title = "Average wOBA"){
  data %>%
    setup_inplay() %>%
    woba_gam_fit()  %>%
    contour_graph(L, title)
}
