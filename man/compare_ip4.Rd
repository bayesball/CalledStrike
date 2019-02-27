\name{compare_ip4}
\alias{compare_ip4}

\title{
  Compare Four In-Play Distributions
}
\description{
  Constructs tile graph of four Statcast datasets
}
\usage{
  compare_ip(df, type = "ls")
}
\arguments{
  \item{df}{
    list of data frames containing four Statcast datasets
    }
  \item{type}{
    abbreviation for variable to be graphed
  }
}
\value{
  Constructs 2 x 2 grid tile plots of the variable (choices for type are launch speed ("ls"), launch angle ("la"), spray angle ("sa"), hit ("h"), and home run ("hr"))
}
\author{
  Jim Albert
}


