swing_gam_fit <- function(d){
  gam(Swing ~ s(plate_x, plate_z),
      family=binomial,
      data=d)
}
