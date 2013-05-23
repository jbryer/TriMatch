\name{merge.triangle.matches}
\alias{merge.triangle.matches}
\title{Merges outcomes with the matched set.}
\usage{
  \method{merge}{triangle.matches} (x, y, ...)
}
\arguments{
  \item{x}{the result of \code{\link{trimatch}}}

  \item{y}{another data frame or vector to merge with.}

  \item{...}{unused}
}
\value{
  \code{x} with the additional column(s) added.
}
\description{
  The \code{y} parameter should be a subset of the orignal
  data used.
}

