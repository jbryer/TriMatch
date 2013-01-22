\name{plot.triangle.psa}
\alias{plot.triangle.psa}
\title{Triangle plot.}
\usage{
  plot.triangle.psa(tpsa, point.alpha = 0.3,
    point.size = 5, legend.title = "Treatment",
    text.size = 4, draw.edges = FALSE,
    draw.segments = TRUE, edge.alpha = 0.2,
    edge.color = "grey",
    edge.labels = c("Model 1", "Model 2", "Model 3"), ...)
}
\arguments{
  \item{tpsa}{the results from \code{\link{trips}}.}

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

