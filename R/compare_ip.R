compare_ip <- function(df1, df2,
                       title1 = "Dataset 1",
                       title2 = "Dataset 2",
                       type = "ls"){
  flag <- TRUE
  ptype <- "m"
  if(type == "ls"){
    df1 %>%
      setup_inplay() %>%
      ls_gam_fit() -> fit1
    df2 %>%
      setup_inplay() %>%
      ls_gam_fit() -> fit2
    gtitle <- "Launch Speed"} else {
      if(type == "la"){
        df1 %>%
          setup_inplay() %>%
          la_gam_fit() -> fit1
        df2 %>%
          setup_inplay() %>%
          la_gam_fit() -> fit2
        gtitle <- "Launch Angle"} else {
          if(type == "sa"){
            df1 %>%
              setup_inplay() %>%
              sa_gam_fit() -> fit1
            df2 %>%
              setup_inplay() %>%
              sa_gam_fit() -> fit2
            gtitle <- "Spray Angle"} else {
              if(type == "h"){
                df1 %>%
                  setup_inplay() %>%
                  hr_h_gam_fit(HR = FALSE) -> fit1
                df2 %>%
                  setup_inplay() %>%
                  hr_h_gam_fit(HR = FALSE) -> fit2
                gtitle <- "In Play Prob(Hit)"
                ptype <- "p"} else {
                  if(type == "hr"){
                    df1 %>%
                      setup_inplay() %>%
                      hr_h_gam_fit(HR = TRUE) -> fit1
                    df2 %>%
                      setup_inplay() %>%
                      hr_h_gam_fit(HR = TRUE) -> fit2
                    gtitle <- "In Play Prob(HR)"
                    ptype <- "p"} else {
                      print("Wrong type")
                      flag <- FALSE
                    }}}}}

  if(flag == TRUE){
    df_p1 <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                         plate_z = seq(1, 4, length=50))
    df_p1$lp <- predict(fit1, df_p1)
    if(ptype == "p"){
      df_p1$lp <- exp(df_p1$lp) / (1 + exp(df_p1$lp))}
    df_p2 <- df_p1
    df_p2$lp <- predict(fit2, df_p2)
    if(ptype == "p"){
      df_p2$lp <- exp(df_p2$lp) / (1 + exp(df_p2$lp))}
    df_p1$Name <- title1
    df_p2$Name <- title2
    df_p <- rbind(df_p1, df_p2)
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
                    fill= lp)) +
      scale_fill_distiller(palette = "Spectral")  +
      geom_path(aes(x, y), data=kZone,
                lwd=1, col="red") +
      facet_wrap(~ Name, ncol = 2) +
      xlim(-1.5, 1.5) +
      ylim(1.0, 4.0)  +
      coord_fixed() +
      ggtitle(gtitle) +
      centertitle() +
      increasefont()
  }
}
