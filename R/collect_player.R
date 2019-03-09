collect_player <- function(player, Season = 2018){
  id <- get_id(player)
  get_sc_data(id, season = Season)
}
