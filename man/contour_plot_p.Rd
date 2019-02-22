\name{contour_plot_p}
\alias{contour_plot_p}

\title{
  Constructs contour plot from gam fit with logistic link
}
\description{
  Constructs contour plot from output from an generalized additive fit with logistic link
}
\usage{
  contour_plot_p(fit, P = 0.5)
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


