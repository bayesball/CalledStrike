\name{swing_contour}
\alias{swing_contour}

\title{
  Swing Contour Plot
}
\description{
  Constructs swing contour plot
}
\usage{
  swing_contour(df, L = seq(0, 1, by = 0.1), title = "Probability of Swing", NCOL = 2)
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
  Constructs a contour plot of the swing probability
}
\author{
  Jim Albert
}


