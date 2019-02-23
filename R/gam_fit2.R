gam_fit2 <- function(d){
  gam(Miss ~ s(plate_x, plate_z),
      family=binomial,
      data=d)
}
