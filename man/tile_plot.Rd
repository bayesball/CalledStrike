\name{tile_plot}
\alias{tile_plot}

\title{
  Constructs heat map plot from GAM fit
}
\description{
  Constructs heat map plot from output from an generalized additive fit
}
\usage{
  tile_plot(df, title)
}
\arguments{
  \item{df}{
    data frame containing Statcast and predicted values
  }
  \item{title}{
    title for the graph
  }
}
\value{
  ggplot2 heat map graph with strike zone added
}
\author{
  Jim Albert
}


