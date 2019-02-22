\name{sa_gam_fit}
\alias{sa_gam_fit}

\title{
  Implements generalized additive fit for spray angle data
}
\description{
  Implements generalized additive fit where SA = s(plate_x, plate_z)
}
\usage{
  sa_gam_fit(d)
}
\arguments{
  \item{d}{
    Statcast data frame with variables spray_angle, plate_x, plate_z
  }
}
\value{
  gam object containing the generalized additive fit
}
\author{
  Jim Albert
}


