% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r_squared_vis.R
\name{plot_tiles}
\alias{plot_tiles}
\title{Tile plot}
\usage{
plot_tiles(
  p,
  percent,
  xmin,
  ymin,
  xmax,
  ymax,
  rev = FALSE,
  color = "black",
  fill = "#F5C710",
  alpha = 0.5
)
}
\arguments{
\item{p}{a ggplot2 expression.}

\item{xmin}{x coordinate for the lower left corner.}

\item{ymin}{y coordinate for the lower left corner.}

\item{xmax}{x coordinate for the upper right corner.}

\item{ymax}{y coordinate for the upper right corner.}

\item{rev}{if TRUE drawing starts from the upper right.}

\item{color}{color of the perimeter of the boxes.}

\item{fill}{the color used to for the fill if length(percent) == 1.}

\item{alpha}{the transparency for the fill color(s).}
}
\description{
This is an internal function.
}
