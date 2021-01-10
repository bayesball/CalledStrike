location_count_compare <- function(d,
                                   Pitcher = pitch_id,
                                   name = "",
                                   side = "R",
                                   ptype = "Fastball",
                                   count_v = "0-0",
                                   NCOL = 2){

  d %>%
    mutate(PitchType = ifelse(pitch_type %in%
                                c("FC", "FF", "FO", "FS", "FT"),
                              "Fastball", "Offspeed"),
           Count = paste(balls, strikes, sep="-"),
           TheCount = paste("Count =", Count)) -> d
  d <- filter(d,
              stand == side,
              PitchType == ptype,
              pitcher == Pitcher,
              Count %in% count_v)

  title <- paste(name, ":", side, ptype)

  ggplot(d, aes(plate_x, plate_z)) +
    geom_density_2d_filled(contour_var = "ndensity") +
    add_zone() +
    xlim(-2.5, 2.5) +
    ylim(0, 5) +
    theme(legend.position = "none") +
    facet_wrap(~ TheCount, ncol = NCOL) +
    theme(text=element_text(size=18)) +
    ggtitle(title) +
    coord_equal() +
    theme(plot.title = element_text(colour = "red",
                                    size = 24,
                                    hjust = 0.5, vjust = 0.8, angle = 0))
}
