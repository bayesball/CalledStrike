\name{ehit_contour}
\alias{ehit_contour}

\title{
  Expected Hit Contour Plot
}
\description{
  Constructs expected hit contour plot for a specific player based on launch speed and launch angle
}
\usage{
  ehit_contour(df, L = seq(0, 1, by = 0.02), title = "Expected Batting Average", NCOL = 2)
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
  Constructs a contour plot of the expected hit probability
}
\author{
  Jim Albert
}


