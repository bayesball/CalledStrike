hit_contour <- function(data,
                        L = seq(0, 1, by = 0.1)){
  data %>%
    setup_inplay() %>%
    hr_h_gam_fit(HR = FALSE) -> fit

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
    scale_fill_distiller(palette="Spectral") +
    add_zone("black") +
    xlim(-1.5, 1.5) +
    ylim(1.0, 4.0)  +
    coord_fixed() +
    centertitle() +
    increasefont()
}
