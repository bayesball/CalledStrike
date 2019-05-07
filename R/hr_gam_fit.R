hr_gam_fit <- function(d, HR = TRUE){
  gam(HR ~ s(launch_angle, launch_speed),
        family=binomial,
        data=d)
}
