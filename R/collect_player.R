collect_player <- function(player, Season = 2018,
                           Batter = TRUE){
  id <- get_id(player)
  get_sc_data(id, season = Season, batter = Batter)
}
