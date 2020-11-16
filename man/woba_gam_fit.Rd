\name{woba_gam_fit}
\alias{woba_gam_fit}

\title{
  Implements generalized additive fit for wOBA data
}
\description{
  Implements generalized additive fit where wOBA = s(plate_x, plate_z)
}
\usage{
  woba_gam_fit(d)
}
\arguments{
  \item{d}{
    Statcast data frame with variables woba_value, plate_x, plate_z
  }
}
\value{
  gam object containing the generalized additive fit
}
\author{
  Jim Albert
}


