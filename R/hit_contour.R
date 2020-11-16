hit_contour <- function(data,
                        L = seq(0, 1, by = 0.1),
                        title = "Probability of a Hit"){
  data %>%
    setup_inplay() %>%
    hr_h_gam_fit(HR = FALSE) %>%
    contour_graph(L, title)
}
