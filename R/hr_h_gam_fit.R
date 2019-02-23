hr_h_gam_fit <- function(d, HR = TRUE){
  if(HR == TRUE){
      gam(HR ~ s(plate_x, plate_z),
        family=binomial,
        data=d)} else{
      gam(H ~ s(plate_x, plate_z),
              family=binomial,
              data=d)}
}
