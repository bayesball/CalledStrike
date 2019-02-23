la_gam_fit <- function(d){
  gam(launch_angle ~ s(plate_x, plate_z),
      data=d)
}
