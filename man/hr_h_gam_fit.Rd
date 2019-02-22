\name{hr_h_gam_fit}
\alias{hr_h_gam_fit}

\title{
  Implements generalized additive fit for in-play data
}
\description{
  Implements generalized additive fit where logit(prob(HR)) = s(plate_x, plate_z)
}
\usage{
  hr_h_gam_fit(d, HR = TRUE)
}
\arguments{
  \item{d}{
    Statcast data frame with variables HR, plate_x, plate_z
  }
  \item{HR}{
    If HR = TRUE, then HR is response, otherwise H is response
  }
}
\value{
  gam object containing the generalized additive fit
}
\author{
  Jim Albert
}


