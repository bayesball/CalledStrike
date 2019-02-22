\name{contour_plot_m}
\alias{contour_plot_m}

\title{
  Constructs contour plot from gam fit
}
\description{
  Constructs contour plot from output from an generalized additive fit
}
\usage{
  contour_plot_m(fit, M)
}
\arguments{
  \item{fit}{
    saved gam fit with continuous response
  }
  \item{M}{
    vector of values for contour levels
  }
}
\value{
  ggplot2 contour graph with strike zone added
}
\author{
  Jim Albert
}


