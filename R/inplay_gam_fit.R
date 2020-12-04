inplay_gam_fit <- function(d){
  gam(InPlay ~ s(plate_x, plate_z),
      family=binomial,
      data=d)
}
