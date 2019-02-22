\name{ls_gam_fit}
\alias{ls_gam_fit}

\title{
  Implements generalized additive fit for launch speed data
}
\description{
  Implements generalized additive fit where LS = s(plate_x, plate_z)
}
\usage{
  ls_gam_fit(d)
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


