\name{plot.loess3}
\alias{plot.loess3}
\title{Loess plot for matched triplets.}
\usage{
  plot.loess3(tmatch, outcome, model, ylab = "Outcome",
    smooth.method = "auto", plot.connections = FALSE,
    connections.color = "black", connections.alpha = 0.2,
    plot.points = geom_point, points.alpha = 0.1,
    points.palette = "Dark2")
}
\arguments{
  \item{tmatch}{the results of \code{\link{trimatch}}.}

  \item{outcome}{a vector representing the outcomes.}

  \item{model}{an integer between 1 and 3 indicating from
  which model the propensity scores will be used.}

  \item{ylab}{the label for the y-axis.}

  \item{smooth.method}{the smoothing method. See
  \code{\link{stat_smooth}} for more info.}

  \item{plot.connections}{boolean indicating whether lines
  will be drawn connecting each matched triplet.}

  \item{connections.color}{the line color of connections.}

  \item{connections.alpha}{number between 0 and 1
  representing the alpha levels for connection lines.}

  \item{plot.points}{a \code{ggplot2} function for plotting
  points. Usually \code{\link{geom_point}} or
  \code{\link{geom_jitter}}. If \code{NULL} no points will
  be drawn.}

  \item{points.alpha}{number between 0 and 1 representing
  the alpha level for the points.}

  \item{points.palette}{the color palette to use. See
  \code{\link{scale_colour_brewer}} and
  \url{http://colorbrewer2.org/} for more information.}
}
\value{
  a \code{ggplot2} figure.
}
\description{
  This function will create a \code{ggplot2} figure with
  propensity scores on the x-axis and the outcome on the
  y-axis. Three Loess regression lines will be plotted
  based upon the propensity scores from \code{model}. Since
  each model produces propensity scores for two of the
  three groups, the propensity score for the third group in
  each matched triplet will be the mean of the other two.
  If \code{model} is not specified, the default will be to
  use the model that estimates the propensity scores for
  the first two groups in the matching order.
}

