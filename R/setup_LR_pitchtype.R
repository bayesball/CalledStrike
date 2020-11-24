setup_LR_pitchtype <- function(sc){
  sc %>% mutate(Pitch_Type =
                  ifelse(pitch_type %in%
                           c("FC", "FF", "FS", "FT", "SI"),
                         "Fastball",
                         ifelse(pitch_type %in%
                                  c("CH", "CU", "KC", "SL"),
                                "Offspeed", "Other")))  %>%
    filter(Pitch_Type != "Other") -> sc

  split(sc, sc$p_throws) -> sc_LR
  sc_new <- c(split(sc_LR[[1]],
                    sc_LR[[1]]$Pitch_Type),
              split(sc_LR[[2]],
                    sc_LR[[2]]$Pitch_Type))

  names(sc_new) <- c("1 - Left - Fastball",
                     "3 - Left - Off-Speed",
                     "2 - Right - Fastball",
                     "4 - Right - Off-Speed")
  sc_new
}
