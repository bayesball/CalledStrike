setup_all <- function(sc){
  swing_situations <- c("hit_into_play",
                        "foul", "swinging_strike",
                        "swinging_strike_blocked",
                        "missed_bunt",
                        "hit_into_play_no_out", "foul_bunt",
                        "foul_tip",
                        "hit_into_play_score")
  sc %>%
    mutate(Swing = ifelse(description %in%
                             swing_situations, 1, 0),
           Count = paste(balls, strikes, sep="-"),
           Pitch_Type =
             ifelse(pitch_type %in%
            c("FC", "FF", "FS", "FT", "SI"),
                    "Fastball",
          ifelse(pitch_type %in%
                    c("CH", "CU", "KC", "SL"),
                           "Offspeed", "Other")),
          Count_Type = ifelse(Count %in%
                  c("2-0", "3-1", "3-0"), "Ahead",
                  ifelse(Count %in%
               c("0-1", "0-2", "1-2", "2-2"),
                      "Behind", "Neutral")))
}
