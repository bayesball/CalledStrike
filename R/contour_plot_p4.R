contour_plot_p4 <- function(df, P = 0.5, type = "ms"){
  # fitting part #####################
  flag <- TRUE
  if(type %in% c("ms", "cs", "h", "hr", "sw") == FALSE){
    print("Wrong type")
    flag <- FALSE
  }
  if(flag == TRUE){
  fit <- vector(mode = "list", length = 4)
  if (type == "sw"){
    for(j in 1:4){
      df[[j]] %>%
        setup_all() %>%
        swing_gam_fit() -> fit[[j]]
      title <- "Swing Rate"
      if(missing(P) == TRUE) P <- c(0.25, 0.5, 0.75)
    }
  }
  if (type == "ms"){
    for(j in 1:4){
      df[[j]] %>%
      setup_swing() %>%
      miss_gam_fit() -> fit[[j]]
      title <- "Missed on Swing Rate"
      if(missing(P) == TRUE) P <- c(0.1, 0.2, 0.3)
    }
  }
  if (type == "cs"){
    for(j in 1:4){
      df[[j]] %>%
        setup_called() %>%
        strike_gam_fit() -> fit[[j]]
        title <- "Called Strike Rate"
        if(missing(P) == TRUE) P <- c(0.5, 0.9)
    }
  }
  if (type == "h"){
    for(j in 1:4){
      df[[j]] %>%
        setup_inplay() %>%
        hr_h_gam_fit(HR = FALSE) -> fit[[j]]
        title <- "In-Play Hit Rate"
        if(missing(P) == TRUE) P <- c(0.2, 0.3, 0.4)
    }
  }
  if (type == "hr"){
    for(j in 1:4){
      df[[j]] %>%
        setup_inplay() %>%
        hr_h_gam_fit(HR = TRUE) -> fit[[j]]
        title <- "In-Play Home Run Rate"
        if(missing(P) == TRUE) P <- c(.05, .1, .15)
    }
  }

  ####################################
  df_p1 <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                      plate_z = seq(1, 4, length=50))
  df_p1$lp <- predict(fit[[1]], df_p1)
  df_p1$Probability <- exp(df_p1$lp) / (1 + exp(df_p1$lp))
  df_p1$Group <- names(df)[1]
  df_p2 <- df_p1
  df_p2$lp <- predict(fit[[2]], df_p2)
  df_p2$Probability <- exp(df_p2$lp) / (1 + exp(df_p2$lp))
  df_p2$Group <- names(df)[2]
  df_p3 <- df_p1
  df_p3$lp <- predict(fit[[3]], df_p3)
  df_p3$Probability <- exp(df_p3$lp) / (1 + exp(df_p3$lp))
  df_p3$Group <- names(df)[3]
  df_p4 <- df_p1
  df_p4$lp <- predict(fit[[4]], df_p4)
  df_p4$Probability <- exp(df_p4$lp) / (1 + exp(df_p4$lp))
  df_p4$Group <- names(df)[4]
  df_p <- rbind(df_p1, df_p2, df_p3, df_p4)

  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.85
  outKzone <- 0.85
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  ggplot(df_p)  +
    stat_contour(aes(x=plate_x, y=plate_z,
                     z=Probability,
                     color = stat(level)),
                 breaks=c(P),
                 size=1.5) +
    geom_path(aes(x, y), data=kZone,
              lwd=1, col="red") +
    xlim(-1.5, 1.5) +
    ylim(1.0, 4.0)  +
    coord_fixed() +
    facet_wrap(~ Group, ncol = 2) +
    ggtitle(title) +
    centertitle() +
    increasefont()
  }
}
