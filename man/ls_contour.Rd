\name{ls_contour}
\alias{ls_contour}

\title{
  Launch Speed Contour Plot
}
\description{
  Constructs launch speed contour graph
}
\usage{
  ls_contour(df, L = seq(75, 105, 5), title = "Launch Velocity")
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
  Constructs a contour plot of the launch speeds
}
\author{
  Jim Albert
}


