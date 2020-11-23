eba_gam_fit <- function(d){
  gam(eba ~ s(plate_x, plate_z),
      data=d)
}
