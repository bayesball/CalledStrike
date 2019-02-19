sa_gam_fit <- function(d){
  require(mgcv)
  gam(spray_angle ~ s(plate_x, plate_z),
      data=d)
}
