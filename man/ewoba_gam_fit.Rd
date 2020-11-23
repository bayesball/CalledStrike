\name{ewoba_gam_fit}
\alias{ewoba_gam_fit}

\title{
  Implements generalized additive fit for expected wOBA data
}
\description{
  Implements generalized additive fit where expected wOBA = s(plate_x, plate_z)
}
\usage{
  ewoba_gam_fit(d)
}
\arguments{
  \item{d}{
    Statcast data frame with variables launch_speed, plate_x, plate_z
  }
}
\value{
  gam object containing the generalized additive fit
}
\author{
  Jim Albert
}


