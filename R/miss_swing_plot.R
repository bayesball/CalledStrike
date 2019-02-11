miss_swing_plot <- function(data, title){
  data %>%
    setup_swing() %>%
    gam_fit2() %>%
    cplot2() +
    ggtitle(title) +
    centertitle()
}
