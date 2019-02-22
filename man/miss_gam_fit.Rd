\name{miss_gam_fit}
\alias{miss_gam_fit}

\title{
  Implements generalized additive fit for swinging data
}
\description{
  Implements generalized additive fit where logit(prob(Miss)) = s(plate_x, plate_z)
}
\usage{
  miss_gam_fit(d)
}
\arguments{
  \item{d}{
    Statcast data frame with variables Miss, plate_x, plate_z
  }
}
\value{
  gam object containing the generalized additive fit
}
\author{
  Jim Albert
}


