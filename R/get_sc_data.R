get_sc_data <- function(id, season = 2018,
                        batter = TRUE){
  dstart <- paste(season, "-03-15", sep="")
  dstop <- paste(season, "-10-15", sep="")
  if(batter == TRUE){
  scrape_statcast_savant(start_date = dstart,
                               end_date = dstop,
                               player_type = "batter",
                               playerid = id)} else {
  scrape_statcast_savant(start_date = dstart,
                               end_date = dstop,
                               player_type = "pitcher",
                               playerid = id)}

}
