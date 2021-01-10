\name{location_count}
\alias{location_count}

\title{
  Graphs Pitch Locations for Single Count
}
\description{
  Graphs the pitch locations for a specific pitcher on a particular value of the count
}
\usage{
  location_count(d, Pitcher = pitch_id, name = "", count = "0-0")
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
  \item{count}{
    Specific count value of interest
  }
}
\value{
  Constructs bivariate density estimates of the pitch location for both batter sides and pitch types on that specific count
}
\author{
  Jim Albert
}


