compute_pitch_values <- function(sc){
  # create Count variable and add run values
  sc %>%
    mutate(Count = paste(balls, strikes,
                         sep="-"))  %>%
    inner_join(count_values,
                  by = c("Count" = "Count")) -> sc

  # create new Count variable -
  sc %>% mutate(new_balls = balls + (type == "B"),
                new_strikes =
                  pmin(2, strikes + (type == "S")),
        new_Count = ifelse(is.na(events) == FALSE,
                          events,
        paste(new_balls, new_strikes, sep="-"))) -> sc

  # add Runs Values of New Counts
  sc %>%
    inner_join(count_values,
          by = c("new_Count" = "Count")) -> sc

  # compute pitch values
  sc %>%
    mutate(Pitch_Value = Runs.y - Runs.x)
}
