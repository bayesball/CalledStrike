\name{la_contour}
\alias{la_contour}

\title{
  Launch Angle Contour Plot
}
\description{
  Constructs launch angle contour graph
}
\usage{
  la_contour(df, L = seq(-10, 40, 10), title = "Launch Angle", NCOL = 2)
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
  Constructs a contour plot of the launch angles
}
\author{
  Jim Albert
}


