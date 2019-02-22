\name{la_gam_fit}
\alias{la_gam_fit}

\title{
  Implements generalized additive fit for launch angle data
}
\description{
  Implements generalized additive fit where LA = s(plate_x, plate_z)
}
\usage{
  la_gam_fit(d)
}
\arguments{
  \item{d}{
    Statcast data frame with variables launch_angle, plate_x, plate_z
  }
}
\value{
  gam object containing the generalized additive fit
}
\author{
  Jim Albert
}


