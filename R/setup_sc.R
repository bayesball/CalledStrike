setup_sc <- function(sc){
  called_situations <- unique(sc$description)[c(2, 5, 8)]
  filter(sc, description %in%
           called_situations) %>%
    mutate(Strike = ifelse(description ==
                             "called_strike", 1, 0),
           Count = paste(balls, strikes, sep="-"))
}
