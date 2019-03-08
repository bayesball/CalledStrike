\name{contour_plot_m4_fill}
\alias{contour_plot_m4_fill}

\title{
  Compare Four In-Play Mean Distributions
}
\description{
  Constructs filled contour graph of four Statcast datasets
}
\usage{
  contour_plot_m4_fill(df, M = 50, type = "ls")
}
\arguments{
  \item{df}{
    list of data frames containing four Statcast datasets
    }
  \item{P}{
    vector of values for contour levels
  }
  \item{type}{
    abbreviation for variable to be graphed
  }
}
\value{
  Constructs 2 x 2 grid of filled contour plots of the variable (choices for type are launch speed ("ls"), launch angle ("la"), and spray angle ("sa"))
}
\author{
  Jim Albert
}


