setup_inplay <- function(sc){
  filter(sc, type == "X") %>%
    mutate(HR = ifelse(events == "home_run", 1, 0),
           H = ifelse(events %in% c("single",
           "double", "triple", "home_run"), 1, 0),
           Count = paste(balls, strikes, sep="-"),
           spray_angle = atan((hc_x - 125.42) /
                  (198.27 - hc_y)) * 180 / pi)
}
