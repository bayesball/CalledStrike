\name{sa_contour}
\alias{sa_contour}

\title{
  Spray Angle Contour Plot
}
\description{
  Constructs spray angle contour graph
}
\usage{
  sa_contour(df, L = seq(-40, 40, by = 10), title = "Spray Angle")
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
  Constructs a contour plot of the spray angles
}
\author{
  Jim Albert
}


