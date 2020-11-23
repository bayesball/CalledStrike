\name{ewoba_contour}
\alias{ewoba_contour}

\title{
  Expected wOBA Contour Plot
}
\description{
  Constructs expected wOBA contour plot for a specific player based on launch speed and launch angle
}
\usage{
  ewoba_contour(df, L = seq(0, 1, by = 0.02), title)
}
\arguments{
  \item{df}{
    data frame or list containing Statcast data
  }
  \item{L}{
    values for contour lines
  }
  \item{title}{
    title of the graph
  }
}
\value{
  Constructs a contour plot of the expected wOBA
}
\author{
  Jim Albert
}


