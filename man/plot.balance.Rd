\name{plot.balance}
\alias{plot.balance}
\title{Balance plot for the given covariate.}
\usage{
  plot.balance(tmatch, covar, model,
    nstrata = attr(attr(tmatch, "triangle.psa"), "nstrata"),
    label = "Covariate", ylab = "", se.ratio = 2)
}
\arguments{
  \item{tmatch}{results from \code{\link{trimatch}}.}

  \item{covar}{vector of the covaraite to check balance
  of.}

  \item{model}{an integer between 1 and 3 indicating from
  which model the propensity scores will be used.}

  \item{nstrata}{number of strata to use.}

  \item{ylab}{label of the y-axis.}

  \item{se.ratio}{a multiplier for how large standard error
  bars will be.}

  \item{label}{label for the legend.}
}
\value{
  a \code{ggplot2} figure.
}
\description{
  If the covariate is numeric, boxplots will be drawn with
  red points for the mean and green error bars for the
  standard error. For non-numeric covariates a barplot will
  be drawn.
}
\details{
  A Friedman rank sum test will be performed for all
  covariate types, printed, and stored as an attribute to
  the returned object named \code{friedman}. If a
  continuous covariate a repeated measures ANOVA will also
  be performed, printed, and returned as an attribute named
  \code{rmanova}.
}

