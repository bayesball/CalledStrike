select_sc_var <- function(sc){
  select(sc, player_name, pitch_type, plate_x,
         plate_z, events, description,
         launch_angle, launch_speed,
         hc_x, hc_y, balls, type,
         strikes, game_year, stand,
         woba_value, p_throws,
         estimated_ba_using_speedangle,
         estimated_woba_using_speedangle)
}
