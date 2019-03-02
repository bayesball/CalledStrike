\name{compare_plot_m4}
\alias{compare_plot_m4}

\title{
  Compare Four In-Play Mean Distributions
}
\description{
  Constructs contour graph of four Statcast datasets
}
\usage{
  compare_plot_m4(df, M = 50, type = "ls")
}
\arguments{
  \item{df}{
    list of data frames containing four Statcast datasets
    }
  \item{P}{
    vector of values for contour levels
  }
  \item{type}{
    abbreviation for variable to be graphed
  }
}
\value{
  Constructs 2 x 2 grid tile plots of the variable (choices for type are launch speed ("ls"), launch angle ("la"), and spray angle ("sa"))
}
\author{
  Jim Albert
}


