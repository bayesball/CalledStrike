\name{called_strike_contour}
\alias{called_strike_contour}

\title{
  Called Strike Contour Plot
}
\description{
  Constructs called strike contour plot for a specific player
}
\usage{
  called_strike_contour(df, L = c(0.5, 0.9), title, NCOL = 2)
}
\arguments{
  \item{df}{
    data frame or list containing Statcast data
  }
  \item{L}{
    values of the contour lines
  }
  \item{title}{
    title of the graph
  }
  \item{NCOL}{
    number of columns in multipanel display
  }
}
\value{
  Constructs a contour plot of the called strike probability
}
\author{
  Jim Albert
}


