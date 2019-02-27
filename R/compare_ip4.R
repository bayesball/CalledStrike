compare_ip4 <- function(df, type = "ls"){
flag <- TRUE
ptype <- "m"
fit <- vector(mode = "list", length = 4)
if(type == "ls"){
  for(j in 1:4){
    df[[j]] %>%
      setup_inplay() %>%
      ls_gam_fit() -> fit[[j]]
  }
  gtitle <- "Launch Speed"} else {
  if(type == "la"){
    for(j in 1:4){
      df[[j]] %>%
        setup_inplay() %>%
        la_gam_fit() -> fit[[j]]
    }
    gtitle <- "Launch Angle"} else {
    if(type == "sa"){
      for(j in 1:4){
        df[[j]] %>%
          setup_inplay() %>%
          sa_gam_fit() -> fit[[j]]
      }
      gtitle <- "Spray Angle"} else {
    if(type == "h"){
      for(j in 1:4){
        df[[j]] %>%
          setup_inplay() %>%
          hr_h_gam_fit(HR = FALSE) -> fit[[j]]
      }
      gtitle <- "In Play Prob(Hit)"
      ptype <- "p"} else {
    if(type == "hr"){
      for(j in 1:4){
        df[[j]] %>%
          setup_inplay() %>%
          hr_h_gam_fit(HR = TRUE) -> fit[[j]]
      }
      gtitle <- "In Play Prob(HR)"
      ptype <- "p"} else {
        print("Wrong type")
        flag <- FALSE
      }}}}}


if(flag == TRUE){
  df_p1 <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                      plate_z = seq(1, 4, length=50))
  df_p1$lp <- predict(fit[[1]], df_p1)
  if(ptype == "p"){
    df_p1$lp <- exp(df_p1$lp) / (1 + exp(df_p1$lp))}
  df_p2 <- df_p1
  df_p2$lp <- predict(fit[[2]], df_p2)
  if(ptype == "p"){
    df_p2$lp <- exp(df_p2$lp) / (1 + exp(df_p2$lp))}
  df_p3 <- df_p1
  df_p3$lp <- predict(fit[[3]], df_p3)
  if(ptype == "p"){
    df_p3$lp <- exp(df_p3$lp) / (1 + exp(df_p3$lp))}
  df_p4 <- df_p1
  df_p4$lp <- predict(fit[[4]], df_p4)
  if(ptype == "p"){
    df_p4$lp <- exp(df_p4$lp) / (1 + exp(df_p4$lp))}

  if(length(names(df)) == 0){
    names(df) <- paste("Dataset", 1:4, sep=" ")
  }
  df_p1$Name <- names(df)[1]
  df_p2$Name <- names(df)[2]
  df_p3$Name <- names(df)[3]
  df_p4$Name <- names(df)[4]
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
