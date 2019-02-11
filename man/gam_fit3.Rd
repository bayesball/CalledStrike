\name{gam_fit3}
\alias{gam_fit3}

\title{
  Implements generalized additive fit for in-play data
}
\description{
  Implements generalized additive fit where logit(prob(HR)) = s(plate_x, plate_z)
}
\usage{
  gam_fit3(d, HR = TRUE)
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


