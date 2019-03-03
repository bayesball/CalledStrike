\name{swing_gam_fit}
\alias{swing_gam_fit}

\title{
  Implements generalized additive fit for swing data
}
\description{
  Implements generalized additive fit where logit(Swing) = s(plate_x, plate_z)
}
\usage{
  swing_gam_fit(d)
}
\arguments{
  \item{d}{
    Statcast data frame with variables Swing, plate_x, plate_z
  }
}
\value{
  gam object containing the generalized additive fit
}
\author{
  Jim Albert
}


