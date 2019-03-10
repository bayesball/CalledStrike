\name{compare_contour}
\alias{compare_contour}

\title{
  Compare Rate or Mean Distributions
}
\description{
  Constructs filled contour graphs of one more Statcast datasets
}
\usage{
  compare_contour(df, P = 0.5, type = "ms")
}
\arguments{
  \item{df}{
    list of data frames containing Statcast datasets
    }
  \item{P}{
    vector of contour levels
  }
  \item{type}{
    abbreviation for variable to be graphed
  }
}
\value{
  Constructs 2 x 2 grid of filled contour plots of the variable (choices for type are called strike ("cs"), swing ("sw"), miss swing ("ms"), hit ("h"), home run ("hr"), launch angle ("la"), launch speed ("ls"), and spray angle ("sa"))
}
\author{
  Jim Albert
}


