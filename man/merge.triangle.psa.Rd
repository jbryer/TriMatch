\name{merge.triangle.psa}
\alias{merge.triangle.psa}
\title{Merges covariate(s) with the results of \code{\link{trips}}.}
\usage{
  \method{merge}{triangle.psa} (x, y, ...)
}
\arguments{
  \item{x}{the result of \code{\link{trips}}}

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

