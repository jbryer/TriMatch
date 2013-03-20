\name{boxdiff.plot}
\alias{boxdiff.plot}
\title{Returns a \code{ggplot2} box plot of the differences.}
\usage{
  boxdiff.plot(tmatch, out, plot.mean = TRUE)
}
\arguments{
  \item{tmatch}{the results from \code{\link{trimatch}}.}

  \item{out}{a vector of the outcome measure of interest.}

  \item{plot.mean}{logical indicating whether the means
  should be plotted.}
}
\value{
  a \code{ggplot2} boxplot of the differences.
}
\description{
  A boxplot of differences between each pair of treatments.
}

