setup_called <- function(sc){
  require(dplyr)
  called_situations <- c("blocked ball", "ball",
                         "called_strike")
  filter(sc, description %in%
           called_situations) %>%
    mutate(Strike = ifelse(description ==
                             "called_strike", 1, 0),
           Count = paste(balls, strikes, sep="-"))
}
