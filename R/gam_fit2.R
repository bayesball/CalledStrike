gam_fit2 <- function(d){
  require(mgcv)
  gam(Miss ~ s(plate_x, plate_z),
      family=binomial,
      data=d)
}
