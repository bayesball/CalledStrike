\name{cplot}
\alias{cplot}

\title{
  Constructs contour plot from gam fit
}
\description{
  Constructs contour plot from output from an generalized additive fit
}
\usage{
  cplot(fit, P)
}
\arguments{
  \item{fit}{
    saved gam fit with binary response
  }
  \item{P}{
    vector of probabilities for contour levels
  }
}
\value{
  ggplot2 contour graph with strike zone added
}
\author{
  Jim Albert
}


