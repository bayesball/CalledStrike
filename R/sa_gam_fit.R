sa_gam_fit <- function(d){
  gam(spray_angle ~ s(plate_x, plate_z),
      data=d)
}
