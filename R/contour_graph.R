contour_graph <- function(fit, L, title){
  grid <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                      plate_z = seq(1, 4, length=50))
  grid$lp <- predict(fit, grid, type = "response")

  ggplot(grid)  +
    geom_contour_fill(aes(x=plate_x,
                          y=plate_z,
                          z=lp),
                      breaks=c(L),
                      size=1.5) +
    scale_fill_distiller(palette="Spectral") +
    add_zone("black") +
    xlim(-1.5, 1.5) +
    ylim(1.0, 4.0)  +
    coord_fixed() +
    ggtitle(title) +
    centertitle() +
    increasefont()
}
