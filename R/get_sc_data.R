get_sc_data <- function(id, season = 2018){
  require(baseballr)
  dstart <- paste(season, "-03-15", sep="")
  dstop <- paste(season, "-10-15", sep="")
  scrape_statcast_savant(start_date = dstart,
                               end_date = dstop,
                               player_type = "batter",
                               playerid = id)
}
