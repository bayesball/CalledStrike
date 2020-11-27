split_pitchtype <- function(sc){
  sc %>% mutate(Pitch_Type =
                  ifelse(pitch_type %in%
                        c("FC", "FF", "FS", "FT", "SI"),
                         "Fastball",
                         ifelse(pitch_type %in%
                        c("CH", "CU", "KC", "SL"),
                                "Offspeed", "Other")))  %>%
    filter(Pitch_Type != "Other") -> sc

  split(sc, sc$Pitch_Type)
}
