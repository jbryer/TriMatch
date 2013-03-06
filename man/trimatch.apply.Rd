\name{trimatch.apply}
\alias{trimatch.apply}
\title{Recursive function to find possible matched triplets using the apply functions.}
\usage{
  trimatch.apply(tpsa, caliper, d1, d2, d3, exact1, exact2,
    exact3, nmatch, status = TRUE)
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

  \item{caliper}{a vector of length one or three indicating
  the caliper to use for matching within each step. This is
  expressed in standardized units such that .25 means that
  matches must be within .25 of one standard deviation to
  be kept, otherwise the match is dropped.}

  \item{nmatch}{number of closest matches to retain before
  moving to next edge. This can be \code{Inf} in which case
  all matches within the caliper will be retained through
  to the next step. For large datasets, evaluating all
  possible matches within the caliper could be time
  consuming.}

  \item{status}{whether to print a status bar while
  executing.}
}
\description{
  Internal method.
}

