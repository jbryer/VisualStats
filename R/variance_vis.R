#' Variance Visualization
#'
#' This function is adapted from Bruce Dudek's Shiny app available here:
#' https://shiny.rit.albany.edu/stat/visualizess/
#'
#' @author Jason Bryer
#' @author Bruce Dudek
#' @return a ggplot2 expression.
#' @param x a vector of values to display the variance of.
#' @param plot_deviances either a logical (TRUE to include all cross
#'        products) or an integer vector indicating which cross products from
#'         \code{x} are to be plotted.#'
#' @param variance_position where to plot the sample and/or population
#'        variances. Values can be 'top', 'bottom', or 'middle'.
#' @examples
#' 	x <- c(97.88, 107.91, 88.26, 115.21, 87.38)
#' 	variance_vis(x)
#' @export
variance_vis <- function(x,
						 plot_mean = TRUE,
						 plot_deviances = TRUE,
						 plot_deviances_x = FALSE,
						 plot_deviances_y = FALSE,
						 plot_population_variance = TRUE,
						 plot_sample_variance = FALSE,
						 plot_population_sd = FALSE,
						 plot_sample_sd = FALSE,
						 point_size = 3,
						 variance_position = 'top',
						 deviation_col = 'blue',
						 sample_variance_col = 'green',
						 population_variance_col = 'orange',
						 sample_sd_col = 'darkgreen',
						 population_sd_col = 'darkorange',
						 sd_size = 1.5,
						 xlab = '',
						 ...
) {
	xlim <- c(min(x) - .1 * diff(range(x)),
			  max(x) + .1 * diff(range(x)))
	ylim <- c(0, diff(range(xlim)))

	df <- data.frame(x = x,
					 stringsAsFactors = FALSE)
	mean_x <- mean(x)
	df$y <- abs(df$x - mean_x)
	df$abs_diff <- abs(df$x - mean_x)
	population_variance <- sum(df$abs_diff) / (nrow(df) )
	sample_variance <- sum(df$abs_diff) / (nrow(df) - 1)

	if(is.logical(plot_deviances)) {
		if(plot_deviances) {
			plot_deviances <- 1:nrow(df)
		} else {
			plot_deviances <- c()
		}
	}

	if(is.logical(plot_deviances_x)) {
		if(plot_deviances_x) {
			plot_deviances_x <- 1:nrow(df)
		} else {
			plot_deviances_x <- c()
		}
	}

	if(is.logical(plot_deviances_y)) {
		if(plot_deviances_y) {
			plot_deviances_y <- 1:nrow(df)
		} else {
			plot_deviances_y <- c()
		}
	}

	p <- ggplot(df)

	if(plot_mean) {
		p <- p + geom_vline(xintercept = mean_x, color = 'black', linetype = 2)
	}

	get_y_position <- function(width) {
		if(variance_position == 'top') {
			return(c(ylim[2] - width, ylim[2]))
		} else if(variance_position == 'middle') {
			return(c(ylim[2] / 2 - width / 2, ylim[2] / 2 + width / 2))
		} else if(variance_position == 'bottom') {
			return(c(0, width))
		} else {
			warning('Unsupported variance_position specified.')
			return(c(0, width))
		}
	}

	if(plot_sample_variance) {
		y_pos <- get_y_position(sample_variance)
		p <- p + geom_rect(
			xmin = mean_x - sample_variance / 2,
			xmax = mean_x + sample_variance / 2,
			ymin = y_pos[1],
			ymax = y_pos[2],
			color = sample_variance_col,
			fill = sample_variance_col,
			alpha = 0.05)
	}

	if(plot_population_variance) {
		y_pos <- get_y_position(population_variance)
		p <- p + geom_rect(
			xmin = mean_x - population_variance / 2,
			xmax = mean_x + population_variance / 2,
			ymin = y_pos[1],
			ymax = y_pos[2],
			color = population_variance_col,
			fill = population_variance_col,
			alpha = 0.05)
	}

	if(length(plot_deviances) > 0) {
		p <- p + geom_rect(
			data = df[plot_deviances,,drop = FALSE],
			aes(xmin = mean_x, xmax = x, ymin = 0, ymax = y),
			color = deviation_col, fill = deviation_col, alpha = 0.05)
	}

	if(length(plot_deviances_x) > 0) {
		p <- p + geom_segment(
			data = df[plot_deviances_x,,drop = FALSE],
			aes(x = mean_x, y = 0, xend = x, yend = 0),
			color = deviation_col,
			arrow = arrow(),
			size = 1.5
		)
	}

	if(length(plot_deviances_y) > 0) {
		p <- p + geom_segment(
			data = df[plot_deviances_y,,drop = FALSE],
			aes(x = x, y = 0, xend = x, yend = y),
			color = deviation_col,
			size = 1.5
		)
	}

	if(plot_population_sd) {
		y_pos <- get_y_position(population_variance)
		p <- p + geom_errorbarh(aes(xmin = mean_x - population_variance / 2,
									xmax = mean_x + population_variance / 2,
									y = mean(y_pos)),
								color = population_sd_col,
								size = sd_size)
	}

	p <- p + geom_point(y = 0, aes(x = x), size = point_size) +
		xlim(xlim) + ylim(ylim) +
		theme(axis.text.y = element_blank(),
			  axis.ticks.y = element_blank()) +
		xlab(xlab) + ylab('') +
		coord_equal()

	return(p)
}

if(FALSE) {
	library(ggplot2)
	x <- c(97.88, 107.91, 88.26, 115.21, 87.38)

	variance_vis(x,
				 plot_mean = TRUE,
				 plot_deviances = FALSE,
				 plot_deviances_x = which(x == max(x)),
				 plot_deviances_y = which(x == max(x)),
				 plot_sample_variance = FALSE,
				 plot_population_variance = FALSE,
				 plot_population_sd = FALSE)

	variance_vis(x,
				 plot_mean = TRUE,
				 plot_deviances = which(x == max(x)),
				 plot_deviances_x = which(x == max(x)),
				 plot_deviances_y = which(x == max(x)),
				 plot_sample_variance = FALSE,
				 plot_population_variance = FALSE,
				 plot_population_sd = FALSE)


	variance_vis(x,
				 plot_mean = TRUE,
				 plot_deviances = FALSE,
				 plot_sample_variance = FALSE,
				 plot_population_variance = TRUE,
				 plot_population_sd = TRUE)

	variance_vis(x,
				 plot_mean = TRUE,
				 plot_deviances = TRUE,
				 plot_sample_variance = TRUE,
				 plot_population_variance = TRUE,
				 variance_position = 'top')

	variance_vis(x,
				 variance_position = 'bottom',
				 plot_mean = TRUE,
				 plot_deviances = TRUE,
				 plot_sample_variance = FALSE,
				 plot_population_variance = TRUE)


	variance_vis(x,
				 plot_mean = TRUE,
				 plot_deviances = which(x == max(x)), # Largest cross product
				 plot_sample_variance = FALSE,
				 plot_population_variance = TRUE)

	variance_vis(x,
				 plot_mean = TRUE,
				 plot_deviances = c(which(x == max(x)), which(x == min(x))), # Largest cross product
				 plot_sample_variance = FALSE,
				 plot_population_variance = TRUE)

}
