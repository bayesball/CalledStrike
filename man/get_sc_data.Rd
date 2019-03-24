\name{get_sc_data}
\alias{get_sc_data}

\title{
  Scrapes Statcast data
}
\description{
  Scrapes Statcast data for a specific hitter
}
\usage{
  get_sc_data(id, season = 2018, batter = TRUE)
}
\arguments{
  \item{id}{
    MLBAM player id
  }
  \item{season}{
    Season
  }
  \item{batter}{
   should batter or pitcher data be collected
  }
}
\value{
  Statcast data for player for particular season
}
\author{
  Jim Albert
}


