pitch_value_contour <- function(df,
                                L = seq(-0.2, 0.2, by = 0.01),
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
    contour_graph(df_p, L, title)
  } else {
    contour_graph(df_p, L, title) +
      facet_wrap(~ Group, ncol = NCOL)
  }
}
