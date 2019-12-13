\name{compare_contour}
\alias{compare_contour}

\title{
  Compare Rate or Mean Distributions
}
\description{
  Constructs filled contour graphs of one more Statcast datasets
}
\usage{
  compare_contour(df, L = 0.5, type = "ms", Ncol = 2, etitle = "")
}
\arguments{
  \item{df}{
    list of data frames containing Statcast datasets
    }
  \item{L}{
    vector of contour levels
  }
  \item{type}{
    abbreviation for variable to be graphed
  }
  \item{Ncol}{
    number of columns in faceted display
  }
  \item{etitle}{
    additional text to add to title of graph
  }
}
\value{
  Constructs 2 x 2 grid of filled contour plots of the variable (choices for type are called strike ("cs"), swing ("sw"), make contact on  swing ("mc"), miss swing ("ms"), hit ("h"), home run ("hr"), launch angle ("la"), launch speed ("ls"), and spray angle ("sa"))
}
\author{
  Jim Albert
}


