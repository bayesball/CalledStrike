home_run_contour <- function(data,
               L = seq(.04, .24, by = 0.04),
               title = "Probability of Home Run"){
  data %>%
    setup_inplay() %>%
    hr_h_gam_fit(HR = TRUE)  %>%
    contour_graph(L, title)
}
