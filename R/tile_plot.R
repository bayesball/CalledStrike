tile_plot <- function(fit, title){
  df_p <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                      plate_z = seq(1, 4, length=50))
  df_p$lp <- predict(fit, df_p, type = "response")

  ggplot(df_p)  +
    geom_tile(data=df_p,
              aes(x=plate_x, y=plate_z,
                  fill= lp)) +
    scale_fill_distiller(palette = "Spectral")  +
    add_zone("black") +
    xlim(-1.5, 1.5) +
    ylim(1.0, 4.0)  +
    coord_fixed() +
    ggtitle(title) +
    centertitle() +
    increasefont()
}
