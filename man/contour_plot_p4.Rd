\name{contour_plot_p4}
\alias{contour_plot_p4}

\title{
  Compare Four In-Play Rate Distributions
}
\description{
  Constructs contour graph of four Statcast datasets
}
\usage{
  contour_plot_p4(df, P = 0.5, type = "ms")
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
  Constructs 2 x 2 grid contour plots of the variable (choices for type are called strike ("cs"), miss swing ("ms"), hit ("h"), and home run ("hr"))
}
\author{
  Jim Albert
}


