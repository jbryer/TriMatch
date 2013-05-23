\name{plot.balance.plots}
\alias{plot.balance.plots}
\title{Prints a grid of balance plots.}
\usage{
  \method{plot}{balance.plots} (x, rows, cols,
    byrow = TRUE, plot.sequence = seq_along(bplots), ...)
}
\arguments{
  \item{x}{the results of \code{\link{balance.plot}} when a
  data frame is specified.}

  \item{rows}{if \code{covar} is a data frame of
  covariates, the number of rows in the grid of figures.}

  \item{cols}{if \code{covar} is a data farme of
  covariates, the number of columns in the grid of
  figures.}

  \item{byrow}{if TRUE (default), plots will be drawn by
  rows, otherwise by columns.}

  \item{plot.sequence}{the sequence (or subset) of plots to
  draw.}

  \item{...}{currenlty unused.}
}
\description{
  Prints a grid of balance plots.
}

