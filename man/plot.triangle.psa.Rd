\name{plot.triangle.psa}
\alias{plot.triangle.psa}
\title{Triangle plot.}
\usage{
  \method{plot}{triangle.psa} (x, point.alpha = 0.3,
    point.size = 5, legend.title = "Treatment",
    text.size = 4, draw.edges = FALSE,
    draw.segments = TRUE, edge.alpha = 0.2,
    edge.color = "grey",
    edge.labels = c("Model 1", "Model 2", "Model 3"),
    sample = c(1), ...)
}
\arguments{
  \item{x}{the results from \code{\link{trips}}.}

  \item{point.alpha}{alpha level for points.}

  \item{point.size}{point size.}

  \item{legend.title}{title for the legend.}

  \item{text.size}{text size.}

  \item{draw.edges}{draw edges of the triangle.}

  \item{draw.segments}{draw segments connecting points
  across two models.}

  \item{edge.alpha}{alpha level for edges if drawn.}

  \item{edge.color}{the color for edges if drawn.}

  \item{edge.labels}{the labels to use for each edge of the
  triangle.}

  \item{sample}{a vector of length 1 or 3 representing the
  sample of points to plot.  The position of each element
  corresponds to the groups as returned by
  \code{attr(tpsa,'groups')}. If equal to one, all points
  will be plotted.  Values less than one will plot a
  percentage of points. Values greater than one exactly
  that number of points will be plotted.}

  \item{...}{currently unused.}
}
\value{
  ggplot2 figure
}
\description{
  Triangle plot showing the fitted values (propensity
  scores) for three different models.
}
\seealso{
  triangle.psa
}

