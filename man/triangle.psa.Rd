\name{triangle.psa}
\alias{triangle.psa}
\title{Estimates propensity scores for three groups (e.g. two treatments and one control).}
\usage{
  triangle.psa(thedata, treat, ids, groups = unique(treat),
    nstrata = 5, ...)
}
\arguments{
  \item{thedata}{the data frame.}

  \item{treat}{vector or factor indicating the
  treatment/control assignment for \code{thedata}. Length
  must be equal to \code{nrow(thedata)}.}

  \item{ids}{a vector that uniquely identify each row. This
  will be passed through to the results to assist in
  matching. If NULL, \code{row.names(thedata)} will be
  used.}

  \item{groups}{a vector of exactly length three
  corresponding the values in \code{treat} for each
  control/treatment.}

  \item{nstrata}{the number of strata marks to plot on the
  edge.}

  \item{...}{other parameters passed to \code{\link{glm}}.}
}
\description{
  Estimates propensity scores for three groups (e.g. two
  treatments and one control).
}

