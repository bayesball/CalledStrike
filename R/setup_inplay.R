setup_inplay <- function(sc){
  require(dplyr)
  filter(sc, type == "X") %>%
    mutate(HR = ifelse(events == "home_run", 1, 0),
           H = ifelse(events %in% c("single",
           "double", "triple", "home_run"), 1, 0),
           Count = paste(balls, strikes, sep="-"))
}
