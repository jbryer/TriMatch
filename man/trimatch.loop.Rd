\name{trimatch.loop}
\alias{trimatch.loop}
\title{Recursive function to find possible matched triplets using nested for loops.}
\usage{
  trimatch.loop(tpsa, d1, d2, d3, exact1, exact2, exact3,
    nmatch)
}
\arguments{
  \item{d1}{matrix of distances between groups A and B}

  \item{d2}{matrix of distances between groups B and C}

  \item{d3}{matrix of distances between groups C and A}

  \item{exact1}{matrix of exact matches between groups A
  and B}

  \item{exact2}{matrix of exact matches between groups B
  and C}

  \item{exact3}{matrix of exact matches between groups C
  and A}

  \item{tpsa}{the results from \code{\link{trips}}}

  \item{nmatch}{number of closest matches to retain before
  moving to next edge. This can be \code{Inf} in which case
  all matches within the caliper will be retained through
  to the next step. For large datasets, evaluating all
  possible matches within the caliper could be time
  consuming.}
}
\description{
  Internal method.
}

