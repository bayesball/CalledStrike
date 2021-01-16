\name{pitch_value_contour}
\alias{pitch_value_contour}

\title{
  Pitch Value Contour Plot
}
\description{
  Constructs pitch value contour plot
}
\usage{
  pitch_value_contour(df,
                      L = seq(-0.2, 0.2, by = 0.01),
                      title = "Pitch Value",
                      NCOL = 2)
}
\arguments{
  \item{df}{
    data frame or list containing Statcast data with a PitchValue variable
  }
  \item{L}{
    values of the contour lines
  }
  \item{title}{
    title of the graph
  }
  \item{NCOL}{
    number of columns in multipanel display
  }
}
\value{
  Constructs a contour plot of the estimated pitch value from the gam model fit
}
\author{
  Jim Albert
}


