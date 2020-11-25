\name{ewoba_contour}
\alias{ewoba_contour}

\title{
  Expected wOBA Contour Plot
}
\description{
  Constructs expected wOBA contour plot for a specific player based on launch speed and launch angle
}
\usage{
  ewoba_contour(df, L = seq(0, 1, by = 0.02), title = "Expected wOBA", NCOL = 2)
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
  \item{NCOL}{
    number of columns in multipanel display
  }
}
\value{
  Constructs a contour plot of the expected wOBA
}
\author{
  Jim Albert
}


