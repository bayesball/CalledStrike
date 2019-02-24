\name{compare_ip}
\alias{compare_ip}

\title{
  Compare Two In-Play Distributions
}
\description{
  Constructs tile graph of two Statcast datasets
}
\usage{
  compare_ip(df1, df2, title1 = "Dataset 1",
                       title2 = "Dataset 2",
                       type = "ls")
}
\arguments{
  \item{df1}{
    data frame containing first Statcast dataset
  }
  \item{df2}{
    data frame containing second Statcast dataset
  }
  \item{title1}{
    title for first dataset
  }
  \item{title2}{
    title for second dataset
  }
  \item{type}{
    abbreviation for variable to be graphed
  }
}
\value{
  Constructs a side-by-side tile plot of the variable (choices are launch speed ("ls"), launch angle ("la"), spray angle ("sa"), hit ("h"), and home run ("hr"))
}
\author{
  Jim Albert
}


