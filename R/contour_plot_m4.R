contour_plot_m4 <- function(df, M = 50, type = "ls"){
  # fitting part #####################
  flag <- TRUE
  if(type %in% c("ls", "la", "sa") == FALSE){
    print("Wrong type")
    flag <- FALSE
  }
  if(flag == TRUE){
  fit <- vector(mode = "list", length = 4)
  if (type == "ls"){
    for(j in 1:4){
      df[[j]] %>%
        setup_inplay() %>%
        ls_gam_fit() -> fit[[j]]
      title <- "Launch Speed"
      if(missing(M) == TRUE) M <- c(80, 90, 100)
    }
  }
  if (type == "la"){
    for(j in 1:4){
      df[[j]] %>%
        setup_inplay() %>%
        la_gam_fit() -> fit[[j]]
        title <- "Launch Angle"
        if(missing(M) == TRUE) M <- c(-10, 0, 20, 30)
    }
  }
  if (type == "sa"){
    for(j in 1:4){
      df[[j]] %>%
        setup_inplay() %>%
        sa_gam_fit() -> fit[[j]]
        title <- "Spray Angle"
        if(missing(M) == TRUE) M <- c(-20, 0, 20)
    }
  }

  ####################################
  df_p1 <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                      plate_z = seq(1, 4, length=50))
  df_p1$lp <- predict(fit[[1]], df_p1)
  df_p1$Group <- names(df)[1]
  df_p2 <- df_p1
  df_p2$lp <- predict(fit[[2]], df_p2)
  df_p2$Group <- names(df)[2]
  df_p3 <- df_p1
  df_p3$lp <- predict(fit[[3]], df_p3)
  df_p3$Group <- names(df)[3]
  df_p4 <- df_p1
  df_p4$lp <- predict(fit[[4]], df_p4)
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
                     z=lp,
                     color = stat(level)),
                 breaks=c(M),
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
