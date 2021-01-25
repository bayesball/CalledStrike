\name{sc2019_pv}
\alias{sc2019_pv}

\title{
  2019 Statcast pitch value data
}
\description{
  Statcast pitch value data for the 2019 season
}
\usage{
  sc2019_pv
}
\format{
   A data frame with 732471 observations on the following 8 variables.
  \describe{
   \item{pitch_type}{type of pitch derived from Statcast}
  \item{plate_x}{horizontal position of ball when it crosses plate from catcher's perspective}
  \item{plate_z}{vertical position of ball when it crosses plate from catcher's perspective}
  \item{stand}{side of plate batter is standing}
  \item{p_throws}{pitcher throwing arm}
  \item{Count}{ball-strike count}
  \item{new_Count}{count or end-of-PA event after pitch}
  \item{Pitch_Value}{value of pitch}
  }
}
\source{
Baseball Savant
}


