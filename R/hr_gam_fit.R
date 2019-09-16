hr_gam_fit <- function(d){
  gam(HR ~ s(launch_angle, launch_speed),
        family=binomial,
        data=d)
}
