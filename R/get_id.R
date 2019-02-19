get_id <- function(name){
  require(baseballr)
  names <- unlist(strsplit(name, " "))
  playerid_lookup(last_name = names[2],
                  first_name = names[1]) %>%
    select(mlbam_id) %>% pull()
}
