get_bbid <- function(name){
  names <- unlist(strsplit(name, " "))
  playerid_lookup(last_name = names[2],
                  first_name = names[1]) %>%
    select(bbref_id) %>% top_n(-1) %>% pull()
}
