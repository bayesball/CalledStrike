contact_gam_fit <- function(d){
  gam(Contact ~ s(plate_x, plate_z),
      family=binomial,
      data=d)
}
