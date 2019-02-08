\name{gam_fit}
\alias{gam_fit}

\title{
  Implements generalized additive fit for called pitch data
}
\description{
  Implements generalized additive fit where logit(prob(Strike)) = s(plate_x, plate_z)
}
\usage{
  gam_fit(d)
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


