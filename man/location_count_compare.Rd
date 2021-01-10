\name{location_count_compare}
\alias{location_count_compare}

\title{
  Graphs Pitch Locations for Several Counts
}
\description{
  Graphs the pitch locations for a specific pitcher for several values of the count
}
\usage{
  location_count_compare(d,  Pitcher = pitch_id,
                             name = "",
                             side = "R",
                             ptype = "Fastball",
                             count_v = "0-0",
                             NCOL = 2)
}
\arguments{
  \item{d}{
    data frame containing Statcast data
  }
  \item{Pitcher}{
    MLBAM id for the pitcher
  }
  \item{name}{
    Name of the pitcher
  }
  \item{side}{
    Batting side, either "R" or "L"
  }
  \item{ptype}{
    Type of pitch, either "Fastball" or "Off-Speed"
  }
  \item{count_v}{
    Vector of count values of interest
  }
}
\value{
  Constructs bivariate density estimates of the pitch location across the group of selected counts
}
\author{
  Jim Albert
}


