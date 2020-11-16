woba_gam_fit <- function(d){
  gam(as.numeric(woba_value) ~
        s(plate_x, plate_z),
      data=d)
}
