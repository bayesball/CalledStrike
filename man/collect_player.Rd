\name{collect_player}
\alias{collect_player}

\title{
  Collects Statcast data for one player
}
\description{
  Collects Statcast data for one player for one season
}
\usage{
  collect_player(player, Season = 2018, Batter = TRUE)
}
\arguments{
  \item{player}{
    name of player
  }
  \item{Season}{
   season of interest
  }
  \item{Batter}{
   should batter or pitcher data be collected
  }
}
\value{
  Statcast dataset for player for one season
}
\author{
  Jim Albert
}


