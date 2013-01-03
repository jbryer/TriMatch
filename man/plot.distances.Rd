\name{plot.distances}
\alias{plot.distances}
\title{Barplot for the sum of distances.}
\usage{
  plot.distances(tmatch, caliper = 0.25)
}
\arguments{
  \item{tmatch}{the results of
  \code{\link{triangle.match}}.}

  \item{caliper}{a vector indicating where vertical lines
  should be drawn as a factor of the standard deviation.
  Rosenbaum and Rubin (1985) suggested one quarter of one
  standard deviation.}
}
\description{
  Barplot for the sum of distances.
}
\seealso{
  triangle.match
}

