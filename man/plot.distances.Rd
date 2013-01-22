\name{plot.distances}
\alias{plot.distances}
\title{Barplot for the sum of distances.}
\usage{
  plot.distances(tmatch, caliper = 0.25, label = FALSE)
}
\arguments{
  \item{tmatch}{the results of \code{\link{trimatch}}.}

  \item{caliper}{a vector indicating where vertical lines
  should be drawn as a factor of the standard deviation.
  Rosenbaum and Rubin (1985) suggested one quarter of one
  standard deviation.}

  \item{label}{label the bars that exceed the minimum
  caliper.}
}
\description{
  Barplot for the sum of distances.
}
\seealso{
  triangle.match
}

