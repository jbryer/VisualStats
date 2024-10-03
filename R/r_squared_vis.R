#' Regression Sum of Squared Visualization
#'
#' @param df data.frame with the data to plot.
#' @param formu formula for the regression.
#' @param x_lab label for the x-axis.
#' @param y_lab label for the y-axis.
#' @param plot_total_variance plot a square representing the total variance in the dependent variable.
#' @param plot_all_variances plot a squares representing all the variances (i.e. each predictor and error).
#' @param plot_error_variance plot a square representing the error/residual variance.
#' @param plot_regression_variance plot a square representing the total regression variance.
#' @param plot_unit_line plot the unit line (i.e. y = x).
#' @param plot_points plot the data points.
#' @param plot_means plot horizontal and vertical lines for the means.
#' @param plot_residuals plot vertical lines representing the residuals.
#' @param plot_residuals_squared plot squares representing the squared residuals.
#' @param variance_alpha the alpha level (transparency) of the residual squares.
#' @param total_variance_color color representing the total variance.
#' @param error_variance_color color representing the error variance.
#' @param regression_variance_color color representing variance explained (i.e. regression variance).
#' @param point_color color of the data points.
#' @param point_alpha the alpha level (transparency) of the data points.
#' @return a ggplot2 expression.
#' @export
#' @importFrom latex2exp TeX
#' @examples
#' df <- VisualStats::simulate(n = 100, r_squared = .5)
#' formu <- y ~ x1 + x2
#' lm(formu, df) |> summary()
#' VisualStats::r_squared_vis(df, formu,
#' 						   plot_total_variance = TRUE,
#' 						   plot_error_variance = FALSE,
#' 						   plot_all_variances = TRUE,
#' 						   plot_residuals_squared = FALSE,
#' 						   plot_residuals = FALSE)
r_squared_vis <- function(df, formu,
						  x_lab = 'Observed Value',
						  y_lab = 'Predicted Value',
						  plot_total_variance = TRUE,
						  plot_all_variances = TRUE,
						  plot_error_variance = FALSE,
						  plot_regression_variance = FALSE,
						  plot_unit_line = TRUE,
						  plot_points = TRUE,
						  plot_means = TRUE,
						  plot_residuals = FALSE,
						  plot_residuals_squared = FALSE,
						  variance_alpha = 0.2,
						  total_variance_color = '#999999',
						  error_variance_color = '#ff7f00',
						  regression_variance_color = '#377eb8',
						  point_color = 'grey50',
						  point_alpha = 0.5
) {
	df <- as.data.frame(df)

	lm_out <- lm(formu, df)
	df$residual <- resid(lm_out)
	df$predicted <- predict(lm_out)
	df$y <- df[,all.vars(formu)[1]]

	total_ss <- sum((df$y - mean(df$y))^2)
	error_ss <- sum((df$y - df$predicted)^2)
	regression_ss <- total_ss - error_ss

	r_squared <- regression_ss / total_ss
	aov_out <- anova(lm_out)
	percent_variance <- aov_out$`Sum Sq` / total_ss
	percents <- percent_variance
	all_vars <- all.vars(formu)
	names(percents) <- c(all_vars[2:length(all_vars)], 'residuals')

	total_ms <- total_ss / (nrow(df))
	# error_ms <- regression_ss / (nrow(df) - length(all.vars(formu)))
	error_ms <- aov_out$`Mean Sq`[length(aov_out$`Mean Sq`)]
	# NOTE: This is technically not mean square, but by dividing by n the
	# ratio to total mean square preserves the R-squared value
	regression_ms <- regression_ss / nrow(df)

	# regression_ms / total_ms
	# error_ms / total_ms

	p <- ggplot(df, aes(x = y, y = predicted)) +
		xlab(x_lab) + ylab(y_lab) +
		coord_equal()

	# TODO: edit colors

	if(plot_total_variance) {
		p <- p + geom_rect(xmin = mean(df$y) - sqrt(total_ms),
						   xmax = mean(df$y) + sqrt(total_ms),
						   ymin = mean(df$predicted) - sqrt(total_ms),
						   ymax = mean(df$predicted) + sqrt(total_ms),
						   fill = total_variance_color,
						   alpha = variance_alpha)
	}

	add_error_variance <- function(p) {
		if(plot_error_variance) {
			p <- p + geom_rect(xmin = mean(df$y) - sqrt(error_ms),
							   xmax = mean(df$y) + sqrt(error_ms),
							   ymin = mean(df$predicted) - sqrt(error_ms),
							   ymax = mean(df$predicted) + sqrt(error_ms),
							   fill = error_variance_color,
							   alpha = variance_alpha)
		}
		return(p)
	}

	add_regression_variance <- function(p) {
		if(plot_regression_variance) {
			p <- p + geom_rect(xmin = mean(df$y) - sqrt(regression_ms),
							   xmax = mean(df$y) + sqrt(regression_ms),
							   ymin = mean(df$predicted) - sqrt(regression_ms),
							   ymax = mean(df$predicted) + sqrt(regression_ms),
							   fill = regression_variance_color,
							   alpha = variance_alpha)
		}
		return(p)
	}

	if(error_ms > regression_ms) {
		p <- add_error_variance(p)
		p <- add_regression_variance(p)
	} else {
		p <- add_regression_variance(p)
		p <- add_error_variance(p)
	}

	if(plot_all_variances) {
		tile_palette <- vs_palette_qual[-c(1, 5)]
		if((length(percents) - 1) > length(tile_palette)) {
			warning('There are more predictors than colors. Some colors will be reused.')
		}
		tile_palette <- c(tile_palette[seq_len(length(percents) - 1)], error_variance_color)

		p <- plot_tiles(p,
						percents = percents,
						xmin = mean(df$y) - sqrt(total_ms),
						xmax = mean(df$y) + sqrt(total_ms),
						ymin = mean(df$predicted) - sqrt(total_ms),
						ymax = mean(df$predicted) + sqrt(total_ms),
						colors = tile_palette,
						alpha = 1)
	}

	if(plot_unit_line) {
		p <- p + geom_abline(slope = 1, intercept = 0)
	}

	if(plot_points) {
		p <- p + geom_point(color = point_color, alpha = point_alpha)
	}

	if(plot_means) {
		p <- p +
			geom_hline(yintercept = mean(df$predicted), linetype = 2) +
			geom_vline(xintercept = mean(df$y), linetype = 2)
	}

	if(plot_residuals) {
		p <- p + geom_segment(aes(x = y, y = predicted, xend = y, yend = y))
	}

	if(plot_residuals_squared) {
		p <- p + geom_rect(data = df,
						   aes(xmin = y, xmax = y + residual, ymin = y, ymax = predicted),
						   alpha = 0.2)
	}

	if(!plot_residuals_squared) {
		max_val <- max(c(df$predicted, df$y))
		min_val <- min(c(df$predicted, df$y))
		p <- p + xlim(c(min_val, max_val)) + ylim(c(min_val, max_val))
	}

	title_str <-  paste0("$R^2 = ", round(r_squared * 100, digits = 0), '$%')
	p <- p + ggtitle(latex2exp::TeX(title_str))

	p <- p + theme_minimal() +
		theme(legend.position = 'bottom') +
		guides(fill = guide_legend(title = ""))

	return(p)
}

#' Tile plot
#'
#' This is an internal function.
#'
#' @param p a ggplot2 expression.
#' @param xmin x coordinate for the lower left corner.
#' @param ymin y coordinate for the lower left corner.
#' @param xmax x coordinate for the upper right corner.
#' @param ymax y coordinate for the upper right corner.
#' @param rev if TRUE drawing starts from the upper right.
#' @param colors color of the perimeter of the boxes.
#' @param fill the color used to for the fill if length(percents) == 1.
#' @param alpha the transparency for the fill color(s).
plot_tiles <- function(p, percents, xmin, ymin, xmax, ymax,
					   rev = FALSE,
					   colors = vs_palette_qual[seq_len(length(percents))],
					   alpha = 0.5) {
	if(any(percents > 1)) {
		percents <- percents / 100
	}
	n_boxes <- round(percents * 100)
	if(sum(n_boxes) <= 0) { return() } # Nothing to draw
	box_width <- (xmax - xmin) / 10
	box_height <- (ymax - ymin) / 10
	x_pos <- seq(xmin, xmax - box_width, by = box_width)
	y_pos <- seq(ymin, ymax - box_height, by = box_height)

	df_boxes <- data.frame(id = 1:100,
						   x = rep(x_pos, times = 10),
						   y = rep(y_pos, each = 10),
						   width = box_width,
						   height = box_height)
	group <- character()
	for(i in seq_len(length(n_boxes))) {
		group <- c(group, rep(names(n_boxes)[i], n_boxes[i]))
	}
	if(length(group) > nrow(df_boxes)) { # Deal with rounding error
		group <- group[1:nrow(df_boxes)]
	} else if(length(group) < nrow(df_boxes)) {
		group <- c(group, rep(group[length(group)], nrow(df_boxes) - length(group)))
	}
	df_boxes$group <- group

	if(length(percents) > 1) {
		names(colors) <- names(percents)
		p <- p + geom_tile(data = df_boxes, aes(x = x + (box_width / 2),
												y = y + (box_height / 2),
												fill = group),
						   alpha = alpha, color = 'black') +
			scale_fill_manual(values = colors, breaks = names(percents))

	} else {
		if(rev) {
			df_boxes <- df_boxes[seq(from = 100, to = 100 - n_boxes + 1),]
		} else {
			df_boxes <- df_boxes[seq_len(n_boxes),]
		}
		p <- p + geom_tile(data = df_boxes, aes(x = x + (box_width / 2),
												y = y + (box_height / 2)),
					  color = colors[1], fill = fill, alpha = alpha)
	}

	return(p)
}

#' Simulate a data frame for multiple regression.
#'
#' @param n number of observations.
#' @param beta beta coefficients. Position one is the intercept, the rest are predictors.
#' @param r_squared desired R-squared value.
#' @return a data.frame.
#' @export
#' @examples
#' get_r_squared <- function(desired) {
#'     model <- lm(y ~ x1 + x2, data = simulate(r_squared = desired, n = 100))
#'     return(summary(model)$r.squared)
#' }
#' df <- data.frame(desired_r_squared = seq(from = 0.05, to = 0.95, by = 0.05))
#' df$actual_r_squared <- sapply(df$desired_r_squared, FUN = get_r_squared)
#' plot(df)
#' abline(a=0, b=1, col="red", lty=2)
simulate <- function(n = 100,
					 beta = c(5, 3, -2),
					 r_squared = 0.8) {
	# Adapted from:
	# https://stackoverflow.com/questions/19096983/when-simulating-multivariate-data-for-regression-how-can-i-set-the-r-squared-e
	stopifnot(length(beta) == 3) # TODO: Would be nice to allow for an arbitrary number of variables
	df <- data.frame(x1 = rnorm(n), x2 = rnorm(n))  # x1 and x2 are independent
	var.epsilon <- (beta[2]^2 + beta[3]^2) * (1 - r_squared) / r_squared
	stopifnot(var.epsilon > 0)
	df$epsilon <- rnorm(n, sd=sqrt(var.epsilon))
	df$y <- with(df, beta[1] + beta[2]*x1 + beta[3]*x2 + epsilon)
	df$epsilon <- NULL
	return(df)
}


if(FALSE) {
	library(ggplot2)
	library(VisualStats)
	df <- VisualStats::simulate(n = 100, r_squared = .6)
	formu <- y ~ x1 + x2
	lm_out$effects
	# The many ways to calculate R^2
	summary(lm_out)$r.squared
	cor(df$y, predict(lm_out))^2
	total_variance <- sum((df$y - mean(df$y))^2) / (nrow(df) - 1)
	var(df$y)

	error_variance <- sum((predict(lm_out) - df$y)^2) / (nrow(df) - 1)
	1 - error_variance / total_variance

	lm_out |> summary()
	lm_out |> anova()
	r_squared_vis(df, formu,
				  plot_total_variance = TRUE,
				  plot_error_variance = TRUE,
				  plot_regression_variance = TRUE,
				  plot_all_variances = TRUE,
				  plot_residuals_squared = FALSE,
				  plot_residuals = TRUE)

	VisualStats::variance_vis(df$y)
}
