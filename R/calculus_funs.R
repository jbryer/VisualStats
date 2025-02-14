#' Calculate Riemann sums
#'
#' @param fun function used to calculate the intergral using Riemann sums.
#' @param min minimum x-value.
#' @param max maximum x-value.
#' @param n number of rectangles used to estimate Riemann sum.
#' @param ... other parameters passed to f.
#' @return data.frame with three variables: xmin, height (i.e. f(xmin)), and area.
#' @export
riemann <- function(fun, min, max, n = 2, ...) {
	width <- (max - min) / n
	boxes <- tibble::tibble(
		xmin = seq(min, min + (n-1) * width, by = width),
		height = fun(xmin, ...),
		area = height * width
	)
	return(boxes)
}

#' Normal distribution function
#' @param x x value.
#' @param mean the mean.
#' @param sd the standard deviation.
#' @return y value for the given x.
#' @export
normal_fun <- function(x, mean = 0, sd = 1) {
	1 / (sd * sqrt(2 * pi)) * exp(1)^(-1/2 * ( (x - mean) / sd )^2)
}

#' Integral plot using Riemann sums.
#'
#' @param fun function used to calculate Riemann sums.
#' @param xmin minimum x-value.
#' @param xmax maximum x-value.
#' @param view_xmin the minimum x-value to view.
#' @param view_xmax the maximum x-value to view.
#' @param n number of rectangles used to estimate Riemann sum.
#' @param view_xmin minimum x-value for the plot view.
#' @param view_xmax maximum x-value for the plot view.
#' @param rect_alpha the alpha (transparency) level for rectangles.
#' @param rect_color the color for rectangles.
#' @param rect_fill the fill color for rectangles.
#' @export
integral_plot <- function(fun = normal_fun,
						  xmin = 0,
						  xmax = 1,
						  view_xmin = xmin - 0.5 * (xmax - xmin),
						  view_xmax = xmax + 0.5 * (xmax - xmin),
						  n = 10,
						  rect_alpha = 0.5,
						  rect_color = 'black',
						  rect_fill = 'grey50') {
	boxes <- riemann(fun,
					 min = xmin,
					 max = xmax,
					 n = n)
	width <- abs((xmax - xmin) / n)

	p <- ggplot() +
		geom_rect(data = boxes,
				  aes(xmin = xmin, ymin = 0, xmax = xmin + width, ymax = height),
				  alpha = rect_alpha, color = rect_color, fill = rect_fill) +
		xlim(c(view_xmin, view_xmax)) +
		stat_function(fun = fun) +
		ggtitle(paste0('Aera %~~% ', prettyNum(sum(boxes$area), digits = 3))) +
		theme_vs()
	return(p)
}

#' Derivative plot
#'
#'
#' @param fun function used to calculate the derivative.
#' @param x_value x-coordinate where the derivative is desired.
#' @param delta_x the difference in x used to estimate the derivative.
#' @param view_xmin minimum x-value for the plot view.
#' @param view_xmax maximum x-value for the plot view.
#' @param ... other parameters passed to f.
#' @export
derivative_plot <- function(fun = normal_fun,
							x_value = 1,
							delta_x = 0.1,
							view_xmin = x_value - 2,
							view_xmax = x_value + 2,
							...) {
	df_segment <- data.frame(x1 = x_value + delta_x,
							 x2 = x_value,
							 y1 = fun(x_value + delta_x),
							 y2 = fun(x_value) )

	segment_slope <- (df_segment$y2 - df_segment$y1) / (df_segment$x2 - df_segment$x1)

	p <- ggplot() +
		stat_function(fun = fun) +
		geom_segment(data = df_segment,
					 aes(x = x1, y = y1, xend = x2, yend = y2),
					 color = 'darkgreen', linetype = 2) +
		geom_abline(slope = segment_slope,
					intercept = fun(x_value) - segment_slope * x_value,
					color = 'darkgreen') +
		geom_point(aes(x = df_segment$x1, y = df_segment$y1), color = 'darkgreen', size = 2) +
		geom_point(aes(x = df_segment$x2, y = df_segment$y2), color = 'darkgreen', size = 2) +
		geom_point(aes(x = x_value, y = fun(x_value)), color = 'blue', size = 3) +
		xlim(c(view_xmin, view_xmax)) +
		xlab('x') + ylab('y') +
		ggtitle(paste0('Slope %~~% ', round(segment_slope, digits = 3))) +
		theme_vs()
	return(p)
}
