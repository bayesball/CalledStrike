called_strike_contour <- function(data,
                                  L = c(0.5, 0.9)){
  data %>%
    setup_called() %>%
    strike_gam_fit() -> fit

  grid <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                      plate_z = seq(1, 4, length=50))
  grid$lp <- predict(fit, grid)
  grid$p <- exp(grid$lp) / (1 + exp(grid$lp))

  ggplot(grid)  +
    geom_contour_fill(aes(x=plate_x,
                          y=plate_z,
                          z=p),
                      breaks=c(L),
                      size=1.5) +
    scale_fill_distiller(palette="BuGn") +
    add_zone("black") +
    xlim(-1.5, 1.5) +
    ylim(1.0, 4.0)  +
    coord_fixed() +
    centertitle() +
    increasefont()
}


