ls_gam_fit <- function(d){
  require(mgcv)
  gam(launch_speed ~ s(plate_x, plate_z),
      data=d)
}
