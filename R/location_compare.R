location_compare <- function(df,
                             title = "",
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
    df[[j]] %>%
      mutate(Group = names(df)[j]) -> df_c
    df_p <- rbind(df_p, df_c)
  }

  ggplot(df_p, aes(plate_x, plate_z)) +
    geom_density_2d_filled(contour_var = "ndensity") +
    add_zone() +
    xlim(-2.5, 2.5) +
    ylim(0, 5) +
    theme(legend.position = "none") +
    facet_wrap(~ Group, ncol = NCOL) +
    theme(text=element_text(size=18)) +
    ggtitle(title) +
    coord_equal() +
    theme(plot.title = element_text(colour = "red",
                                    size = 24,
                                    hjust = 0.5,
                                    vjust = 0.8,
                                    angle = 0))
}
