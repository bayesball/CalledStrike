\name{contour_plot_p4_fill}
\alias{contour_plot_p4_fill}

\title{
  Compare Four In-Play Rate Distributions
}
\description{
  Constructs filled contour graph of four Statcast datasets
}
\usage{
  contour_plot_p4_fill(df, P = 0.5, type = "ms")
}
\arguments{
  \item{df}{
    list of data frames containing four Statcast datasets
    }
  \item{P}{
    vector of probabilities for contour levels
  }
  \item{type}{
    abbreviation for variable to be graphed
  }
}
\value{
  Constructs 2 x 2 grid of filled contour plots of the variable (choices for type are called strike ("cs"), miss swing ("ms"), hit ("h"), and home run ("hr"))
}
\author{
  Jim Albert
}


