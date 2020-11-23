ewoba_gam_fit <- function(d){
  gam(ewoba ~ s(plate_x, plate_z),
      data=d)
}
