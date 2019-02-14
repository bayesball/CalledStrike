la_gam_fit <- function(d){
  require(mgcv)
  gam(launch_angle ~ s(plate_x, plate_z),
      data=d)
}
