contact_gam_fit <- function(d){
  gam(Contact ~ s(.data$plate_x, .data$plate_z),
      family=binomial,
      data=d)
}
