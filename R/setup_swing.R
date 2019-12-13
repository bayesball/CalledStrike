setup_swing <- function(sc){
  swing_situations <- c("hit_into_play",
                        "foul", "swinging_strike",
                        "swinging_strike_blocked",
                        "missed_bunt",
                        "hit_into_play_no_out", "foul_bunt",
                        "foul_tip", "hit_into_play_score")
  filter(sc, description %in%
           swing_situations) %>%
    mutate(Miss = ifelse(description %in%
                c("swinging_strike",
                 "swinging_strike_blocked"), 1, 0),
           Contact = 1 - Miss,
           Count = paste(balls, strikes, sep="-"))
}
