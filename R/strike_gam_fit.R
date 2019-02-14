strike_gam_fit <- function(d){
  require(mgcv)
  gam(Strike ~ s(plate_x, plate_z),
      family=binomial,
      data=d)
}
