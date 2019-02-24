tile_plot_p <- function(fit){
  df_p <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                      plate_z = seq(1, 4, length=50))
  df_p$lp <- predict(fit, df_p)
  df_p$Probability <- exp(df_p$lp) / (1 + exp(df_p$lp))
  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.85
  outKzone <- 0.85
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  ggplot(df_p)  +
    geom_tile(data=df_p,
              aes(x=plate_x, y=plate_z,
                  fill= Probability)) +
    scale_fill_distiller(palette = "Spectral")  +
    geom_path(aes(x, y), data=kZone,
              lwd=1, col="red") +
    xlim(-1.5, 1.5) +
    ylim(1.0, 4.0)  +
    coord_fixed()
}
