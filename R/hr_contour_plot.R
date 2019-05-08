hr_contour_plot <- function(fit,
                            P = c(.1, .3, .5, .7, .9),
                            gtype = "f"){
  require(metR)
  df_p <- expand.grid(launch_angle =
                        seq(15, 40, length=50),
                      launch_speed =
                        seq(90, 115, length=50))
  df_p$Probability <- predict(fit, df_p,
                              type = "response")

  if(gtype == "c"){
  ggplot(df_p)  +
    stat_contour(aes(x=launch_angle,
                     y=launch_speed,
                     z=Probability,
                     color = stat(level)),
                 breaks=c(P),
                 size=1.5)
  }
  if(gtype == "f"){
    ggplot(df_p)  +
      geom_contour_fill(aes(x=launch_angle,
                            y=launch_speed,
                            z=Probability),
                        breaks=c(P),
                        size=1.5) +
      scale_fill_distiller(palette="Spectral")
  }
}
