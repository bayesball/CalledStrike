pitch_value_plot <- function(df,
                                title = "Pitch Value",
                                NCOL = 2){
  if(is.data.frame(df) == TRUE) {
    df <- list(df)
    names(df) <- "Group"
  }
  N_df <- length(df)
  if(is.list(df) == TRUE){
    if(length(names(df)) == 0){
      names(df) <- paste("Group", 1:N_df)
    }
  }
  df_p <- NULL
  for(j in 1:N_df){
    gam(Pitch_Value ~ s(plate_x, plate_z),
        data = df[[j]]) %>%
      grid_predict()  %>%
      mutate(lp = -lp) %>%
      mutate(Group = names(df)[j]) -> df_c
    df_p <- rbind(df_p, df_c)
  }
  if(N_df == 1){
    tile_plot(df_p, title)
  } else {
    tile_plot(df_p, title) +
      facet_wrap(~ Group, ncol = NCOL)
  }
}
