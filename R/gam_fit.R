gam_fit <- function(d){
  gam(Strike ~ s(plate_x, plate_z),
      family=binomial,
      data=d)
}
