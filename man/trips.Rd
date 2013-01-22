\name{trips}
\alias{trips}
\title{Estimates propensity scores for three groups (e.g. two treatments and one control).}
\usage{
  trips(thedata, treat, formu = ~., groups = unique(treat),
    nstrata = 5, ...)
}
\arguments{
  \item{thedata}{the data frame.}

  \item{treat}{vector or factor indicating the
  treatment/control assignment for \code{thedata}. Length
  must be equal to \code{nrow(thedata)}.}

  \item{formu}{the logistic regression formula. Note that
  the dependent variable should not be specified and will
  be modified.}

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

