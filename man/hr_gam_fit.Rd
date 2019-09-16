\name{hr_gam_fit}
\alias{hr_gam_fit}

\title{
  Implements generalized additive fit for in-play data
}
\description{
  Implements generalized additive fit where logit(prob(HR)) = s(plate_x, plate_z)
}
\usage{
  hr_gam_fit(d)
}
\arguments{
  \item{d}{
    Statcast data frame with variables HR, plate_x, plate_z
  }
}
\value{
  gam object containing the generalized additive fit
}
\author{
  Jim Albert
}


