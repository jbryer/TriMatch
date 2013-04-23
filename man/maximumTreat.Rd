\name{maximumTreat}
\alias{maximumTreat}
\title{This method will return at least one treatment from groups one and two within
the caliper.}
\usage{
  maximumTreat(tmatch, ...)
}
\arguments{
  \item{tmatch}{initial results from \code{\link{trimatch}}
  that contains all possible matches within the specified
  caliper.}

  \item{...}{currently unused.}
}
\description{
  This method will attempt to return enough rows to use
  each treatment (the first two groups in the matching
  order) at least once. Assuming treat1 is the first group
  in the match order and treat2 the second, all duplicate
  treat1 rows are removed. Next, all treat2 units not in
  present in after removing duplicate treat1 units are
  identified. For each of those treat2 units, the matched
  triplet with the smallest overall distances where treat2
  is one of the mathched units is retained.
}

