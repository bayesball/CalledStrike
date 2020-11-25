\name{contact_swing_contour}
\alias{contact_swing_contour}

\title{
  Contact on Swings Contour Plot
}
\description{
  Constructs contact on swung balls contour graph
}
\usage{
  contact_swing_contour(df, L = seq(0, 1, by = 0.1), title, NCOL = 2)
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
  Constructs a contour plot of the contact on swung balls probability
}
\author{
  Jim Albert
}


