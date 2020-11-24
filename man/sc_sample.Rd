\name{sc_sample}
\alias{sc_sample}

\title{
  Sample of 2019 Statcast data
}
\description{
  Statcast data for ten batters for the 2019 season
}
\usage{
  sc_sample
}
\format{
   A data frame with 21286 observations on the following 19 variables.
  \describe{
  \item{player_name}{player's name}
  \item{plate_x}{horizontal position of ball when it crosses plate from catcher's perspective}
  \item{plate_z}{vertical position of ball when it crosses plate from catcher's perspective}
  \item{pitch_type}{type of pitch derived from Statcast}
  \item{events}{event of the resulting plate appearance}
  \item{description}{description of the pitch}
  \item{launch_angle}{launch angle of batted ball from Statcast}
  \item{launch_speed}{exit velocity of batted ball from Statcast}
  \item{hc_x}{hit coordinate X of batted ball}
  \item{hc_y}{hit coordinate Y of batted ball}
  \item{balls}{pre-pitch number of balls in count}
  \item{strikes}{pre-pitch number of strikes in count}
  \item{type}{shorthand of pitch result}
  \item{game_year}{year that game took place}
  \item{stand}{side of plate batter is standing}
  \item{woba_value}{wOBA value based on result of play}
  \item{p_throws}{hand pitcher throws with}
  \item{estimated_ba_using_speedangle}{estimated Batting Avg based on launch angle and exit velocity}
  \item{estimated_woba_using_speedangle}{stimated wOBA based on launch angle and exit velocity}
  }
}
\source{
Baseball Savant
}


