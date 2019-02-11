home_run_plot <- function(data, title){
  data %>%
    setup_inplay() %>%
    gam_fit3() %>%
    cplot2() +
    ggtitle(title) +
    centertitle()
}
