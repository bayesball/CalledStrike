compare_contour_p <- function(df, P = 0.5,
                                 type = "ms",
                                 Ncol = 2){
  # fitting part #####################
  N_df <- length(df)
  flag <- TRUE
  if(type %in% c("ms", "cs", "h", "hr", "sw") == FALSE){
    print("Wrong type")
    flag <- FALSE
  }
  if(flag == TRUE){
  fit <- vector(mode = "list", length = N_df)
  if (type == "sw"){
    for(j in 1:N_df){
      df[[j]] %>%
        setup_all() %>%
        swing_gam_fit() -> fit[[j]]
      title <- "Swing Rate"
      if(missing(P) == TRUE) P <- c(0.25, 0.5, 0.75)
    }
  }
  if (type == "ms"){
    for(j in 1:N_df){
      df[[j]] %>%
      setup_swing() %>%
      miss_gam_fit() -> fit[[j]]
      title <- "Missed on Swing Rate"
      if(missing(P) == TRUE) P <- c(0.1, 0.2, 0.3)
    }
  }
  if (type == "cs"){
    for(j in 1:N_df){
      df[[j]] %>%
        setup_called() %>%
        strike_gam_fit() -> fit[[j]]
        title <- "Called Strike Rate"
        if(missing(P) == TRUE) P <- c(0.5, 0.9)
    }
  }
  if (type == "h"){
    for(j in 1:N_df){
      df[[j]] %>%
        setup_inplay() %>%
        hr_h_gam_fit(HR = FALSE) -> fit[[j]]
        title <- "In-Play Hit Rate"
        if(missing(P) == TRUE) P <- c(0.2, 0.3, 0.4)
    }
  }
  if (type == "hr"){
    for(j in 1:N_df){
      df[[j]] %>%
        setup_inplay() %>%
        hr_h_gam_fit(HR = TRUE) -> fit[[j]]
        title <- "In-Play Home Run Rate"
       if(missing(P) == TRUE) P <- c(.05, .1, .15)
    }
  }

  ####################################
  grid <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                      plate_z = seq(1, 4, length=50))
  df_p <- NULL
  for(j in 1:N_df){
    df_c <- grid
    df_c$lp <- predict(fit[[j]], df_c)
    df_c$Probability <- exp(df_c$lp) / (1 + exp(df_c$lp))
    df_c$Group <- names(df)[j]
    df_p <- rbind(df_p, df_c)
  }

  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.85
  outKzone <- 0.85
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  ggplot(df_p)  +
      geom_contour_fill(aes(x=plate_x, y=plate_z,
                       z=Probability),
                   breaks=c(P),
                   size=1.5) +
    scale_fill_distiller(palette="Spectral")  +
      geom_path(aes(x, y), data=kZone,
                lwd=1, col="black") +
      xlim(-1.5, 1.5) +
      ylim(1.0, 4.0)  +
      coord_fixed() +
    facet_wrap(~ Group, ncol = Ncol) +
    ggtitle(title) +
    centertitle() +
    increasefont()
  }
}
