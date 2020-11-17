tile_plot <- function(df, title){

  ggplot(df)  +
    geom_tile(aes(x=plate_x, y=plate_z,
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
