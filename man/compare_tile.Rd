\name{compare_tile}
\alias{compare_tile}

\title{
  Compare Rate or Mean Distributions
}
\description{
  Constructs heat map of one more Statcast datasets
}
\usage{
  compare_tile((df, type = "ms", Ncol = 2))
}
\arguments{
  \item{df}{
    list of data frames containing Statcast datasets
    }
  \item{type}{
    abbreviation for variable to be graphed
  }
  \item{Ncol}{
    number of columns in faceted display
  }
}
\value{
  Constructs 2 x 2 grid of heat maps of the variable (choices for type are called strike ("cs"), swing ("sw"), miss swing ("ms"), hit ("h"), home run ("hr"), launch angle ("la"), launch speed ("ls"), and spray angle ("sa"))
}
\author{
  Jim Albert
}


