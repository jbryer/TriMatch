\name{summary.triangle.matches}
\alias{summary.triangle.matches}
\title{Provides a summary of the matched triplets including analysis of outcome
measure if provided.}
\usage{
  \method{summary}{triangle.matches} (object, outcome,
    p = 0.05, ordering = attr(object, "match.order"), ...)
}
\arguments{
  \item{object}{result of \code{\link{trimatch}}.}

  \item{outcome}{vector representing the outcome measure.}

  \item{p}{threshold of the p value to perform a}

  \item{...}{parameters passed to other statistical tests.}

  \item{ordering}{specify the order for doing the paired
  analysis, that is analysis will be conducted as:
  \code{ordering[1] - ordering[2]}, \code{ordering[1] -
  ordering[3]}, and \code{ordering[2] - ordering[3]}.}
}
\value{
  a trimatch.summary object.
}
\description{
  If an outcome measure is provided this function will
  perform a Freidman Rank Sum Test and repeated measures
  ANOVA. If either test has a statistically significant
  difference (as determined by the value of the \code{p}
  parameter), a Pairwise Wilcoxon Rank Sum Test will also
  be provided.
}
\seealso{
  \code{\link{friedman.test}}, \code{\link{ezANOVA}},
  \code{\link{pairwise.wilcox.test}}
}

