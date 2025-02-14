#' Geometry for plotting distributions with ggplot2.
#'
#' This function works like any other `geom_` type ggplot2 function. However,
#' this technically is not a custom geometry but instead returns a list of
#' geometries to plot the various components of the distribution(s). In most
#' instances this can be used within any ggplot2 expression. The primary
#' difference is this function does not use any of the ggplot2 data or aes
#' components.
#'
#' @param x either a numeric vector, list of numeric vectors, or a data frame.
#'        For lists and data frames multiple density plots will be plotted.
#' @param quantile the proportion to shade around the mean. Set to 0 to not
#'        shade any of the distribution.
#' @param cv critical values which define the bounds of the shaded area. If specified the `quantile`
#'        parameter is ignored.
#' @param color the color of the distribution. If plotting more than one
#'        distribution (i.e. `x` is a list or data.frame) then the length of
#'        color should be equal to the length or number of columns in x.
#' @param alpha the transparency level used for shading the quantiles.
#' @param point_size the size of the point at the center. The default is 0
#'        which will exclude the point.
#' @param center_fun the function used to determine the center of the
#'        distribution, typically `mean` or `median.`
#' @param legend_title if `x` is a list of vectors a legend will be added. This
#'        is the title of the legend or set to NULL to exclude the legend.
#' @param ... other parameters passed to `density()`.
#' @return a list of ggplot2 geometries.
#' @import ggplot2
#' @importFrom stats density quantile approxfun
#' @importFrom grDevices palette
#' @export
#' @examples
#' # Plot a single distribution
#' x <- rnorm(n = 1000)
#' ggplot2::ggplot() +
#'     geom_dist(x) +
#'     ggplot2::geom_point(data = data.frame(x = mean(x), y = 0),
#'                         ggplot2::aes(x = x, y = y), pch = 22)
#'
#' # Plot two distributions
#' x1 <- rnorm(1000, mean = 0, sd = 4)
#' x2 <- rnbinom(1000, 10, .4)
#' ggplot2::ggplot() + geom_dist(list(normal = x1, binomial = x2), adjust = 2)
#'
#' # Plot more than one distribution using a data frame
#' data(mtcars)
#' ggplot2::ggplot() + geom_dist(mtcars[,c('mpg', 'wt')], quantile = 0.5)
geom_dist <- function(
		x,
		quantile = 0.95,
		cv,
		color,
		alpha = 0.2,
		point_size = 0,
		center_fun = mean,
		legend_title = 'Distribution',
		...
) {
	# Returns the geoms for one vector
	get_dist_geoms <- function(x, color, cv) {
		if(!is.numeric(x)) {
			stop('Vector must be numeric.')
		}
		d <- density(x, ...)
		dd <- approxfun(d$x, d$y)
		shaded <- d$x >= cv[1] & d$x <= cv[2]
		df <- rbind(
			data.frame(x = cv[1], y = 0),
			data.frame(x = d$x[shaded], y = d$y[shaded]),
			data.frame(x = cv[2], y = 0)
		)

		area <- NULL
		dens <- NULL
		segment <- NULL
		point <- NULL
		if(quantile > 0) {
			area <- geom_area(data = df,
							  mapping = aes(x = x, y = y),
							  fill = color,
							  alpha = alpha)
		}
		dens <- geom_path(data = data.frame(x = d$x, y = d$y),
						  mapping = aes(x = x, y = y),
						  color = color)
		if(!is.null(center_fun)) {
			segment <- geom_segment(data = df[1,],
									color = color,
									x = center_fun(x),
									y = 0,
									xend = center_fun(x),
									yend = dd(center_fun(x)))
			point <- geom_point(data = df[1,],
								color = color,
								x = center_fun(x),
								y = dd(center_fun(x)),
								size = point_size)
		}
		list(area, dens, segment, point)
	}

	geoms <- list()
	if(is.data.frame(x)) {
		tmp <- list()
		for(i in 1:ncol(x)) {
			tmp[[length(tmp) + 1]] <- x[,i,drop=TRUE]
		}
		names(tmp) <- names(x)
		x <- tmp
	}
	if(is.list(x)) {
		if(missing(cv)) {
			cv <- list()
			for(i in 1:length(x)) {
				cv[[i]] <- quantile(x[[i]], probs = c((1 - quantile) / 2, (1 - quantile) / 2 + quantile))
			}
		} else if(!is.list(cv) | length(cv) != length(x)) {
			stop('Critical values (cv parameter) were specified but is not equal to the length of x.')
		}
		if(missing(color)) {
			color <- palette('ggplot2')[2:(length(x) + 1)]
		} else if(length(color) != length(x)) {
			stop('Length of color and x must be the same')
		}
		if(is.null(names(x))) {
			names(x) <- paste0('dist', 1:length(x))
		}
		if(!all(names(x) %in% names(color))) {
			names(color) <- names(x)
		}
		geoms <- list()
		for(i in 1:length(x)) {
			geoms <- c(geoms, get_dist_geoms(x[[i]],
											 color = color[i],
											 cv = cv[[i]]))
		}
		if(!is.null(legend_title)) {
			# Note: this is a bit of a hack. I need another geometry that defines the color and fill
			# inside aes so the legend will be drawn. The size = 0 so nothing is actually drawn.
			geoms[[length(geoms) + 1]] <- geom_rect(
				data = data.frame(group = names(color),
								  xmin = mean(x[[1]]),
								  xmax = mean(x[[1]]),
								  ymin = 0,
								  ymax = 0),
				mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = group, fill = group),
				linewidth = 0)
			geoms[[length(geoms) + 1]] <- scale_colour_manual(name = legend_title, values = color)
			geoms[[length(geoms) + 1]] <- scale_fill_manual(name = legend_title, values = color)
		}
	} else {
		if(missing(cv)) {
			cv <- quantile(x, probs = c((1 - quantile) / 2, (1 - quantile) / 2 + quantile))
		}
		if(missing(color)) {
			color <- 'black'
		}
		geoms <- get_dist_geoms(x, color = color, cv = cv)
	}

	return(geoms)
}
