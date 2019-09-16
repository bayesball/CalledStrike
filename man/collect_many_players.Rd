\name{collect_many_players}
\alias{collect_many_players}

\title{
  Collects Statcast data for many players
}
\description{
  Collects Statcast data for many players for one season
}
\usage{
  collect_many_players(player_list, Season = 2018,
                       Batter = TRUE)
}
\arguments{
  \item{player_list}{
    vector of player names
  }
  \item{Season}{
   season of interest
  }
  \item{Batter}{
   should batter or pitcher data be collected
  }
}
\value{
  List of Statcast datasets for players for one season
}
\author{
  Jim Albert
}


