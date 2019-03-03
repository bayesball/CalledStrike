setup_all <- function(sc){
  swing_situations <- c("hit_into_play",
                        "foul", "swinging_strike",
                        "swinging_strike_blocked",
                        "missed_bunt",
                        "hit_into_play_no_out", "foul_bunt",
                        "foul_tip", "hit_into_play_score")
  sc %>%
    mutate(Swing = ifelse(description %in%
                             swing_situations, 1, 0),
           Count = paste(balls, strikes, sep="-"))
}
