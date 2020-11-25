\name{miss_swing_contour}
\alias{miss_swing_contour}

\title{
  Miss on Swings Contour Plot
}
\description{
  Constructs miss on swung balls contour graph
}
\usage{
  miss_swing_contour(df, L = seq(0, 1, by = 0.1), title = "Probability of Miss", NCOL = 2)
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
  Constructs a contour plot of the miss on swung balls probability
}
\author{
  Jim Albert
}


