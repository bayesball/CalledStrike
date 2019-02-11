hit_plot <- function(data, title){
  data %>%
    setup_inplay() %>%
    gam_fit3(HR = FALSE) %>%
    cplot2() +
    ggtitle(title) +
    centertitle()
}
