collect_many_players <- function(player_list,
                                 season = 2018){
  require(purrr)
  dfs <- map(player_list,
             collect_player, Season = season)
  names(dfs) <- player_list
  dfs
}
