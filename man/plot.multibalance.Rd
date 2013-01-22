\name{plot.multibalance}
\alias{plot.multibalance}
\title{Multiple covariate blance assessment plot.}
\usage{
  plot.multibalance(tpsa, covs, grid = TRUE)
}
\arguments{
  \item{tpsa}{results of \code{\link{trips}}.}

  \item{covs}{a vector or data frame of covariates.}

  \item{grid}{if TRUE, then a grid of three plots for each
  model will be displayed.}
}
\value{
  a \code{ggplot2} figure.
}
\description{
  A graphic based upon \code{\link{cv.bal.psa}} function in
  the \code{PSAgraphics} package. This graphic plots the
  effect sizes for multiple covariated before and after
  propensity score andjustement.
}

