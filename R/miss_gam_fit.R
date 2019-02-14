miss_gam_fit <- function(d){
  require(mgcv)
  gam(Miss ~ s(plate_x, plate_z),
      family=binomial,
      data=d)
}
