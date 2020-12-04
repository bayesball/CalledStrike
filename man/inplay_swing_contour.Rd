\name{inplay_swing_contour}
\alias{inplay_swing_contour}

\title{
  In-play on Swings Contour Plot
}
\description{
  Constructs in-play on swung balls contour graph
}
\usage{
  inplay_swing_contour(df, L = seq(0, 1, by = 0.1), title, NCOL = 2)
}
\arguments{
  \item{df}{
    data frame or list containing Statcast data
  }
  \item{L}{
    values of contour values
  }
  \item{title}{
    title of the graph
  }
  \item{NCOL}{
    number of columns in multipanel display
  }
}
\value{
  Constructs a contour plot of the in-play on swung balls probability
}
\author{
  Jim Albert
}


