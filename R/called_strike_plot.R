called_strike_plot <- function(data, title){
  data %>%
    setup_called() %>%
    gam_fit() %>%
    cplot2() +
    ggtitle(title) +
    centertitle()
}
