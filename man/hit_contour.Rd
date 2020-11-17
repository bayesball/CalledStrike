\name{hit_contour}
\alias{hit_contour}

\title{
  Hit Contour Plot
}
\description{
  Constructs hit contour plot for a specific player
}
\usage{
  hit_contour(df, L = seq(0, 1, by = 0.1), title)
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
  Constructs a contour plot of the hit probability
}
\author{
  Jim Albert
}


