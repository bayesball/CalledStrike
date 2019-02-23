miss_gam_fit <- function(d){
  gam(Miss ~ s(plate_x, plate_z),
      family=binomial,
      data=d)
}
