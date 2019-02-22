\name{strike_gam_fit}
\alias{strike_gam_fit}

\title{
  Implements generalized additive fit for called pitch data
}
\description{
  Implements generalized additive fit where logit(prob(Strike)) = s(plate_x, plate_z)
}
\usage{
  strike_gam_fit(d)
}
\arguments{
  \item{d}{
    Statcast data frame with variables Strike, plate_x, plate_z
  }
}
\value{
  gam object containing the generalized additive fit
}
\author{
  Jim Albert
}


