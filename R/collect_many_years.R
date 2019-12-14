collect_many_years <- function(player, years){
  id <- get_id(player)
  id <- id[length(id)]
  d_all <- vector(mode = "list", length = length(years))
  for(j in 1:length(years)){
    d_all[[j]] <- get_sc_data(id, season = years[j])
  }
  N <- unlist(strsplit(player, " "))
  names(d_all) <- paste(N[2], years, sep = " ")
  d_all
}
