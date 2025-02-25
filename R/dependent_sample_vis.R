#' Dependent sample scatter plot
#'
#' @param df the data.frame containing the data.
#' @param x the name of the column for the x-axis.
#' @param y the name of the column for the y-axis.
#' @param test whether to use the normal distribution (`norm`) or *t*-distribution (`t`).
#' @param conf_level the confidence level for the null hypothesis test.
#' @param height_multiplier a multiplier applied to the sampling distribution to increase
#'        the height.
#' @param plot_mean whether to plot a line for the mean difference.
#' @param plot_unit_line whether to plot the line y = x.
#' @param plot_projections whether to plot lines connecting the raw data points to
#'        a line perpendicular to the unit line representing the distribution of differences.
#' @param plot_differences whether to plot points on aline perpendicular to the unit
#'        line representing the distribution of differences.
#' @param plot_ci whether to plot a confidence interval.
#' @param plot_ci_lines whether to plot to lines corresponding to the confidence interval.
#' @param plot_samp_dist whether to plot the sampling distribution (normal or t depending
#'        on the `test` parameter).
#' @return a ggplot2 expression.
#' @export
#' @import granova
#' @examples
#' data(blood_lead, package = 'granova')
#' dependent_sample_vis(
#' 	   df = blood_lead,
#' 	   test = 'norm', # or 't'
#' 	   conf_level = 0.95,
#' 	   plot_mean = TRUE,
#' 	   plot_unit_line = TRUE,
#' 	   plot_projections = TRUE,
#' 	   plot_differences = TRUE,
#' 	   plot_ci = TRUE,
#' 	   plot_ci_lines = FALSE,
#' 	   plot_samp_dist = TRUE
#' )
dependent_sample_vis <- function(
		df,
		x = names(df)[1],
		y = names(df)[2],
		test = 'norm', # or 't'
		conf_level = 0.95,
		height_multiplier = 1.5 * diff(range(df[,x], na.rm = TRUE)),
		plot_mean = TRUE,
		plot_unit_line = TRUE,
		plot_projections = TRUE,
		plot_differences = TRUE,
		plot_ci = TRUE,
		plot_ci_lines = FALSE,
		plot_samp_dist = TRUE
) {
	if(FALSE) {
		data(anorexia.sub, package = 'granovaGG')
		df <- anorexia.sub
		x <- names(df)[1]
		y <- names(df)[2]
	}

	k <- 7 # How many standard deviations to plot

	df$diff <- df[,y] - df[,x]
	missing <- is.na(df$diff)
	if(sum(missing) > 0) {
		warning(paste0('There are ', sum(missing), ' rows with missing data. They will be removed.'))
		df <- df[!missing,]
	}

	std_err <- sd(df$diff) / sqrt(nrow(df))
	t_result <- t.test(df[,y], df[,x], conf.level = conf_level)
	mean_diff <- mean(df[,y] - df[,x])
	range <- c(min(c(df[,x], df[,y])), max(c(df[,x], df[,y])))
	buff <- diff(range) * 0.1
	range[1] <- range[1] - 0.5 * diff(range)
	range[2] <- range[2] + buff
	min_val <- min(df[,x], df[,y])

	cv <- NA
	path <- data.frame(x = seq(-k * std_err, k * std_err, length.out = 50))
	ci <- c(NA, NA)
	if(test == 'norm') {
		cv <- abs(qnorm( (1 - conf_level) / 2) )
		path$y <- dnorm(path$x, 0, std_err) * height_multiplier
		ci <- c(mean_diff - cv * std_err,
				mean_diff + cv * std_err)
	} else if(test == 't') {
		cv <- abs(qt( (1 - conf_level) / 2, df = nrow(df) - 1))
		path$y <- dt(path$x, df = nrow(df) - 1) * height_multiplier
		ci <- t_result$conf.int
	} else {
		stop(paste0('Unknown test type: ', test, '. Value can be "t" or "norm".'))
	}
	path$x <- path$x + mean_diff

	# ggplot(path, aes(x = x, y = y)) +
	# 	geom_path() +
	# 	geom_vline(xintercept = mean_diff) +
	# 	geom_vline(xintercept = ci[1], linetype = 2) +
	# 	geom_vline(xintercept = ci[2], linetype = 2)

	projection_intercept <- 2 * min_val
	df$x2 <- (projection_intercept - df$diff) / 2
	df$y2 <- df$x2 + df$diff

	path$x2 <- (projection_intercept - path$x) / 2
	path$y2 <- path$x2 + path$x
	path$x2 <- path$x2 + path$y
	path$y2 <- path$y2 + path$y

	path2 <- path[path$x >= ci[1] & path$x <= ci[2],]
	path2 <- rbind(path2[1,], path2, path2[nrow(path2),])
	path2[1,]$x2 <- (projection_intercept - path2[1,]$x) / 2
	path2[1,]$y2 <- path2[1,]$x2 + path2[1,]$x
	path2[nrow(path2),]$x2 <- (projection_intercept - path2[nrow(path2),]$x) / 2
	path2[nrow(path2),]$y2 <- path2[nrow(path2),]$x2 + path2[nrow(path2),]$x

	offset <- diff(range) * 0.05

	p <- ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
		coord_equal() +
		xlim(range) + ylim(range) +
		theme_vs()

	if(plot_ci) {
		p <- p + geom_segment(
			x = (projection_intercept - ci[1] - offset) / 2,
			y = (projection_intercept - ci[1] - offset) / 2 + ci[1],
			xend = (projection_intercept - ci[2] - offset) / 2,
			yend = (projection_intercept - ci[2] - offset) / 2 + ci[2],
			color = 'darkgreen',
			linewidth = 3
		)
	}

	if(plot_ci_lines) {
		p <- p +
			geom_abline(intercept = ci[1], slope = 1, color = 'darkgreen') +
			geom_abline(intercept = ci[2], slope = 1, color = 'darkgreen')
	}

	if(plot_projections) {
		p <- p + geom_segment(aes(xend = x2, yend = y2), linetype = 2, color = 'grey80')
	}

	if(plot_mean) {
		p <- p + geom_abline(intercept = mean_diff, slope = 1, color = 'maroon', linewidth = 1, linetype = 2)
	}

	if(plot_samp_dist) {
		p <- p +
			geom_polygon(data = path2,
						 aes(x = x2, y = y2),
						 fill = 'maroon', color = 'maroon', alpha = 0.25) +
			geom_path(data = path, aes(x = x2, y = y2), color = 'maroon', size = 0.5)
	}

	if(plot_unit_line) {
		p <- p + geom_abline(intercept = 0, slope = 1)
	}

	if(plot_differences) {
		p <- p +
			geom_abline(intercept = 2 * min_val, slope = -1, color = 'blue', alpha = 0.25) +
			geom_point(aes(x = x2, y = y2), fill = 'blue', size = 2, shape = 21, alpha = 0.25) +
			geom_point(aes(x = x2, y = y2), color = 'blue', size = 2, shape = 21)
	}

	p <- p +
		geom_point(size = 2, shape = 21, fill = 'darkgreen', alpha = 0.2) +
		geom_point(size = 2, color = 'black', shape = 21)

	return(p)
}

if(FALSE) {
	library(granovaGG)
	granovagg.ds(anorexia.sub,
				 revc = FALSE,
				 xlab = "Weight after therapy (lbs.)",
				 ylab = "Weight before therapy (lbs.)"
	)

	data(anorexia.sub, package = 'granova')
	dependent_sample_vis(
		df = anorexia.sub,
		test = 't', # or 't'
		conf_level = 0.95,
		plot_mean = TRUE,
		plot_unit_line = TRUE,
		plot_projections = TRUE,
		plot_differences = TRUE,
		plot_ci = TRUE,
		plot_ci_lines = FALSE,
		plot_samp_dist = TRUE
	)

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
