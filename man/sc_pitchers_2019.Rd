\name{sc_pitchers_2019}
\alias{sc_pitchers_2019}

\title{
  2019 Statcast pitch data
}
\description{
  Statcast data for 20 pitchers for the 2019 season
}
\usage{
  sc_pitchers_2019
}
\format{
   A data frame with 64208 observations on the following 7 variables.
  \describe{
  \item{pitcher}{pitcher's Statcast id}
  \item{balls}{pre-pitch number of balls in count}
  \item{strikes}{pre-pitch number of strikes in count}
  \item{plate_x}{horizontal position of ball when it crosses plate from catcher's perspective}
  \item{plate_z}{vertical position of ball when it crosses plate from catcher's perspective}
  \item{pitch_type}{type of pitch derived from Statcast}
  \item{stand}{side of plate batter is standing}
  }
}
\source{
Baseball Savant
}


