#' Scatter plot with likelihood superimposed.
#'
#' @param x the predictor
#' @param y the outcome
#' @param sigma the root mean square error
#' @param k the number of standard deviations to plot
#' @param height the proportion of the x-axis the distribution should occupy.
#'        That is, a height of 0.4 means the width of the distribution will
#'        occupy approximately 40 percent of the plot width.
#' @param pt the point to highlight and draw the likelihood of.
#' @export
plot_likelihood <- function(x, y, height = 0.4, k = 4, pt = 1,
						   intercept = lm(y ~ x)$coefficients[1],
						   slope = lm(y ~ x)$coefficients[2],
						   sigma = sqrt(sum(resid(lm(y ~ x))^2) / length(x)),
						   ...) {
	xvals <- seq(-k * sigma, k * sigma, length.out = 50)
	heightFactor <- height * diff(range(x, na.rm = TRUE)) / max(dnorm(xvals, 0, sigma) / dnorm(0, 0, sigma))
	yvals <- dnorm(xvals, 0, sigma) / dnorm(0, 0, sigma) * heightFactor
	x0 <- x[pt]
	y0 <- slope * x0 + intercept
	path <- data.frame(x = yvals + x0, y = xvals + y0)
	segment <- data.frame(x = x0,
						  y = y0 - k*sigma,
						  xend = x0,
						  yend = y0 + k*sigma)
	segment2 <- data.frame(x = x0,
						   y = y[pt],
						   xend = dnorm(y0 - abs(y[pt]), 0, sigma) / dnorm(0, 0, sigma) * heightFactor + x0,
						   yend = y[pt])
	segment3 <- data.frame(x = x0,
						   y = y0,
						   xend = dnorm(0, 0, sigma) / dnorm(0, 0, sigma) * heightFactor + x0,
						   yend = y0)


	p <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
		geom_point(size = 0.25) +
		geom_abline(intercept = intercept, slope = slope, alpha = 1, color = '#ff7f00') +
		geom_segment(data = segment3, aes(x = x, y = y, xend = xend, yend = yend), alpha = 0.3) +
		geom_segment(data = segment, aes(x = x, y = y, xend = xend, yend = yend), alpha = 0.5) +
		geom_segment(data = segment2, aes(x = x, y = y, xend = xend, yend = yend), color = '#e31a1c') +
		geom_vline(xintercept = x0) +
		geom_point(data = data.frame(x = x[pt], y = y[pt]), color = '#e31a1c', size = 1.5) +
		geom_point(data = data.frame(x = segment2$xend, y = y[pt]), color = '#e31a1c', size = 1.5) +
		geom_path(data = path, aes(x = x, y = y), color = "#33a02c")
	return(p)
}
