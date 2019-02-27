collect_4_years <- function(player){
  id <- get_id(player)
  d_15 <- get_sc_data(id, season = 2015)
  d_16 <- get_sc_data(id, season = 2016)
  d_17 <- get_sc_data(id, season = 2017)
  d_18 <- get_sc_data(id, season = 2018)
  d_all <- list(d_15, d_16, d_17, d_18)
  N <- unlist(strsplit(player, " "))
  names(d_all) <- paste(N[2], 2015:2018, sep = " ")
  d_all
}
