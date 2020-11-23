\name{ehit_contour}
\alias{ehit_contour}

\title{
  Expected Hit Contour Plot
}
\description{
  Constructs expected hit contour plot for a specific player based on launch speed and launch angle
}
\usage{
  ehit_contour(df, L = seq(0, 1, by = 0.02), title)
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
  Constructs a contour plot of the expected hit probability
}
\author{
  Jim Albert
}


