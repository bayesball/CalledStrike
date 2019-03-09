contour_plot_m4_fill <- function(df, M = 50,
                                 type = "ls",
                                 Ncol = 2){
  require(metR)
  N_df <- length(df)
  # fitting part #####################
  flag <- TRUE
  if(type %in% c("ls", "la", "sa") == FALSE){
    print("Wrong type")
    flag <- FALSE
  }
  if(flag == TRUE){
  fit <- vector(mode = "list", length = N_df)
  if (type == "ls"){
    for(j in 1:N_df){
      df[[j]] %>%
        setup_inplay() %>%
        ls_gam_fit() -> fit[[j]]
      title <- "Launch Speed"
      if(missing(M) == TRUE) M <- c(80, 90, 100)
    }
  }
  if (type == "la"){
    for(j in 1:N_df){
      df[[j]] %>%
        setup_inplay() %>%
        la_gam_fit() -> fit[[j]]
        title <- "Launch Angle"
        if(missing(M) == TRUE) M <- c(-10, 0, 20, 30)
    }
  }
  if (type == "sa"){
    for(j in 1:N_df){
      df[[j]] %>%
        setup_inplay() %>%
        sa_gam_fit() -> fit[[j]]
        title <- "Spray Angle"
        if(missing(M) == TRUE) M <- c(-20, 0, 20)
    }
  }

  ####################################
  grid <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                      plate_z = seq(1, 4, length=50))
  df_p <- NULL
  for(j in 1:N_df){
    df_c <- grid
    df_c$lp <- predict(fit[[j]], df_c)
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
                          z=lp),
                      breaks=c(M),
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
