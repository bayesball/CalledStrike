\name{eba_gam_fit}
\alias{eba_gam_fit}

\title{
  Implements generalized additive fit for expected batting average data
}
\description{
  Implements generalized additive fit where expected BA = s(plate_x, plate_z)
}
\usage{
  eba_gam_fit(d)
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


