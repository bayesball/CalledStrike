location_count <- function(d,
                           Pitcher = pitch_id,
                           name = "",
                           count = "0-0"){
  bs <- as.numeric(unlist(strsplit(count, "-")))
  d <- filter(d,
              balls == bs[1],
              strikes == bs[2],
              pitcher == Pitcher)
  d$Stand <- paste("Stand =", d$stand)
  d %>%
    mutate(PitchType = ifelse(pitch_type %in%
                                c("FC", "FF", "FO", "FS", "FT"),
                              "Fastball", "Offspeed")) -> d
  ggplot(d, aes(plate_x, plate_z)) +
    geom_density_2d_filled(contour_var = "ndensity") +
    add_zone() +
    xlim(-2.5, 2.5) +
    ylim(0, 5) +
    theme(legend.position = "none") +
    facet_grid(PitchType ~ Stand) +
    theme(text=element_text(size=18)) +
    ggtitle(paste(name, count, "Count")) +
    theme(plot.title = element_text(colour = "red",
                                    size = 24,
                                    hjust = 0.5, vjust = 0.8, angle = 0))
}
