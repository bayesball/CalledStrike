\name{inplay_gam_fit}
\alias{inplay_gam_fit}

\title{
  Implements generalized additive fit for inplay data
}
\description{
  Implements generalized additive fit for the probability of an inplay event on swing
}
\usage{
  inplay_gam_fit(d)
}
\arguments{
  \item{d}{
    Statcast data frame with variables InPlay, plate_x, plate_z
  }
}
\value{
  gam object containing the generalized additive fit
}
\author{
  Jim Albert
}


