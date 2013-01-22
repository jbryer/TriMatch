\name{unmatched}
\alias{unmatched}
\title{Returns rows from \code{\link{trips}} that were not matched by \code{\link{trimatch}}.}
\usage{
  unmatched(tmatch)
}
\arguments{
  \item{tmatch}{the results of \code{\link{trimatch}}.}
}
\value{
  a data frame of unmatched rows.
}
\description{
  This function returns a subset of \code{\link{trips}}
  that were not matched by \code{\link{trimatch}}. All data
  frame methods work with the returned object but special
  \code{summary} function will provided relevant
  information.
}

