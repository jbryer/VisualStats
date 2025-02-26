% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dependent_sample_vis.R
\name{dependent_sample_vis}
\alias{dependent_sample_vis}
\title{Dependent sample scatter plot}
\usage{
dependent_sample_vis(
  df,
  x = names(df)[1],
  y = names(df)[2],
  test = "norm",
  conf_level = 0.95,
  height_multiplier = 1.5 * diff(range(df[, x], na.rm = TRUE)),
  plot_mean = TRUE,
  plot_unit_line = TRUE,
  plot_projections = TRUE,
  plot_differences = TRUE,
  plot_ci = TRUE,
  plot_ci_lines = FALSE,
  plot_samp_dist = TRUE
)
}
\arguments{
\item{df}{the data.frame containing the data.}

\item{x}{the name of the column for the x-axis.}

\item{y}{the name of the column for the y-axis.}

\item{test}{whether to use the normal distribution (\code{norm}) or \emph{t}-distribution (\code{t}).}

\item{conf_level}{the confidence level for the null hypothesis test.}

\item{height_multiplier}{a multiplier applied to the sampling distribution to increase
the height.}

\item{plot_mean}{whether to plot a line for the mean difference.}

\item{plot_unit_line}{whether to plot the line y = x.}

\item{plot_projections}{whether to plot lines connecting the raw data points to
a line perpendicular to the unit line representing the distribution of differences.}

\item{plot_differences}{whether to plot points on aline perpendicular to the unit
line representing the distribution of differences.}

\item{plot_ci}{whether to plot a confidence interval.}

\item{plot_ci_lines}{whether to plot to lines corresponding to the confidence interval.}

\item{plot_samp_dist}{whether to plot the sampling distribution (normal or t depending
on the \code{test} parameter).}
}
\value{
a ggplot2 expression.
}
\description{
Dependent sample scatter plot
}
\examples{
data(blood_lead, package = 'granova')
dependent_sample_vis(
	   df = blood_lead,
	   test = 'norm', # or 't'
	   conf_level = 0.95,
	   plot_mean = TRUE,
	   plot_unit_line = TRUE,
	   plot_projections = TRUE,
	   plot_differences = TRUE,
	   plot_ci = TRUE,
	   plot_ci_lines = FALSE,
	   plot_samp_dist = TRUE
)
}
