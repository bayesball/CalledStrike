\name{ls_contour}
\alias{ls_contour}

\title{
  Launch Speed Contour Plot
}
\description{
  Constructs launch speed contour graph
}
\usage{
  ls_contour(df, L = seq(75, 105, 5), title = "Launch Velocity", NCOL = 2)
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
  Constructs a contour plot of the launch speeds
}
\author{
  Jim Albert
}


