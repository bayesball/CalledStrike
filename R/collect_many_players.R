collect_many_players <- function(player_list,
                                 Season = 2018,
                                 Batter = TRUE){
  dfs <- map(player_list,
             collect_player,
             Season = Season,
             Batter = Batter)
  names(dfs) <- player_list
  dfs
}
