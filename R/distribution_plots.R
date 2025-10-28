#' Distribution plot
#'
#' @param dist name of the distribution function to plot (e.g. `norm`, `t`).
#' @param color the color to fill in the area.
#' @param limits the limits of the plot.
#' @param cv the critical value(s) to shade.
#' @param tails whether there are one or two sided tails.
#' @param ... other parameters passed to the distribution function.
#' @seealso [plot_distributions()]
#' @export
#' @rdname distribution_plot
#' @examples
#' distribution_plot('norm', cv = c(-1.96, 1.96), limits = c(-4, 4))
#' distribution_plot('binom', cv = c(3), limits = c(0, 10), tails = 'greater', size = 10, prob = 0.35)
distribution_plot <- function(
		dist,
		cv,
		tails = c('no', 'two.sided', 'less', 'greater'),
		limits,
		color = 'steelblue',
		...
) {
	d_fun <- get(paste0('d', dist))
	p_fun <- get(paste0('p', dist))

	if(!missing(cv)) {
		if(tails[1] %in% c('no', 'two.sided') & length(cv) == 1) {
			stop(paste0('cv parameter must have two values when tails = "', tails[], '"'))
		} else if(tails[1] %in% c('less', 'greater') & length(cv) > 1) {
			warning('cv should have one value for tails = "less"')
		}

	}

	# Determine if it is a discrete or continuous distribution
	suppressWarnings({
		x <- seq(floor(limits[1]) + 0.5, ceiling(limits[2]) - 0.5, by = 1)
		out <- d_fun(x, ...)
		is_discrete <- all(out == 0)
	})

	title <- ''
	p <- ggplot() +
		xlim(limits[1] - 1, limits[2] + 1) # Not sure why I have to do this to get the extreme bars to plot

	if(is_discrete) {
		df <- data.frame(x = seq(limits[1], limits[2], 1))
		df$y <- d_fun(df$x, ...)
		p <- p + geom_bar(data = df, aes(x = x, y = y),
						  stat = 'identity',
						  color = 'black', fill = 'white')

		if(!missing(cv)) {
			if(tails[1] == 'no') {
				area <- p_fun(cv[2], ...) - p_fun(cv[1], ...)
				title <- paste("P(", cv[1], "< x <", cv[2], ")",
							   ifelse(area < 0.01,
							   	   '< 0.01',
							   	   paste0('%~~% ', signif(area, digits = 3))))
				p <- p + geom_bar(
					data = df[df$x <= cv[1] & df$x >= cv[2],], aes(x = x, y = y),
					stat = 'identity',
					fill = color
				)
			} else if(tails[1] == 'two.sided') {
				cv <- c(cv[1], -1 * cv[1])
				cv <- cv[order(cv)]
				area <- p_fun(cv[1], ...) + (1 - p_fun(cv[2], ...))
				title <- paste("P(x < ", cv[1], " & x >", cv[2], ")",
							   ifelse(area < 0.01,
							   	   '< 0.01',
							   	   paste0('%~~% ', signif(area, digits = 3))))
				p <- p + geom_bar(
					data = df[df$x <= cv[1] | df$x >= cv[2],], aes(x = x, y = y),
					stat = 'identity',
					fill = color
				)
			} else if(tails[1] == 'less') {
				area <- p_fun(cv[1], ...)
				title <- paste("P(x < ", cv[1], ")",
							   ifelse(area < 0.01,
							   	   '< 0.01',
							   	   paste0(' %~~% ', signif(area, digits = 3))))
				p <- p + geom_bar(
					data = df[df$x <= cv[1],], aes(x = x, y = y),
					stat = 'identity',
					fill = color
				)
			} else if(tails[1] == 'greater') {
				if(length(cv) > 1) {
					warning('cv should have one value for tails = "greater"')
				}
				area <- 1 - p_fun(cv[1], ...)
				title <- paste("P(x > ", cv[1], ")",
							   ifelse(area < 0.01,
							   	   '< 0.01',
							   	   paste0('= ', signif(area, digits = 3))))
				p <- p + geom_bar(
					data = df[df$x >= cv[1],], aes(x = x, y = y),
					stat = 'identity',
					fill = color
				)
			} else {
				stop('Unkown tails parameter')
			}
		}

	} else {
		p <- p + geom_function(fun = d_fun,
							   args = list(...),
							   xlim = limits)
		if(!missing(cv)) {
			if(tails[1] == 'no') {
				area <- p_fun(cv[2], ...) - p_fun(cv[1], ...)
				title <- paste("P(", cv[1], "< x <", cv[2], ")",
							   ifelse(area < 0.01,
							   	      '< 0.01',
							   	      paste0('%~~% ', signif(area, digits = 3))))

				p <- p + stat_function(fun = d_fun,
							  geom = 'area',
							  xlim = cv,
							  args = list(...),
							  fill = color)
			} else if(tails[1] == 'two.sided') {
				if(length(cv) > 1) {
					warning('cv should have one value for tails = "two.sided"')
				}
				cv <- c(cv[1], -1 * cv[1])
				cv <- cv[order(cv)]
				area <- p_fun(cv[1], ...) + (1 - p_fun(cv[2], ...))
				title <- paste("P(x < ", cv[1], " & x >", cv[2], ")",
							   ifelse(area < 0.01,
							   	   '< 0.01',
							   	   paste0('%~~% ', signif(area, digits = 3))))
				p <- p + stat_function(fun = d_fun,
									   geom = 'area',
									   xlim = c(limits[1], cv[1]),
									   args = list(...),
									   fill = color)
				p <- p + stat_function(fun = d_fun,
									   geom = 'area',
									   xlim = c(cv[2], limits[2]),
									   args = list(...),
									   fill = color)
			} else if(tails[1] == 'less') {
				if(length(cv) > 1) {
					warning('cv should have one value for tails = "less"')
				}
				area <- p_fun(cv[1], ...)
				title <- paste("P(x < ", cv[1], ")",
							   ifelse(area < 0.01,
							   	   '< 0.01',
							   	   paste0(' %~~% ', signif(area, digits = 3))))
				p <- p + stat_function(fun = d_fun,
									   geom = 'area',
									   xlim = c(limits[1], cv[1]),
									   args = list(...),
									   fill = color)
			} else if(tails[1] == 'greater') {
				if(length(cv) > 1) {
					warning('cv should have one value for tails = "greater"')
				}
				area <- 1 - p_fun(cv[1], ...)
				title <- paste("P(x > ", cv[1], ")",
							   ifelse(area < 0.01,
							   	   '< 0.01',
							   	   paste0('= ', signif(area, digits = 3))))
				p <- p + stat_function(fun = d_fun,
									   geom = 'area',
									   xlim = c(cv[1], limits[2]),
									   args = list(...),
									   fill = color)
			} else {
				stop('Unkown tails parameter')
			}
		}
	}

	p <- p + ggtitle(title) +
		xlab('') +
		ylab('') +
		theme_vs() +
		theme(axis.text.y = element_blank(),
			  axis.ticks.y = element_blank())

	return(p)
}

#' Binomial Distribution
#'
#' @param size number of trials (zero or more).
#' @param prob probability of success on each trial.
#' @param log logical; if TRUE, probabilities p are given as log(p).
#' @param tail whether to plot the tails.
#' @export
#' @rdname distribution_plot
#' @importFrom stats qbinom
binomial_plot <- function(size = 10,
						  prob = 0.35,
						  log = FALSE,
						  cv = qbinom(0.95, size = size, prob = prob),
						  tail = TRUE,
						  limits = c(0, 10),
						  color = 'steelblue') {
	cv <- cv
	if(!tail) { cv < c(0, cv) }
	distribution_plot(dist = 'binom',
					  size = size,
					  prob = prob,
					  log = log,
					  tails = ifelse(tail, 'greater', 'no'),
					  cv = cv,
					  limits = limits,
					  color = color)
}

#' Plot a beta distribution
#'
#' @param shape1 non-negative parameters of the Beta distribution.
#' @param shape2 non-negative parameters of the Beta distribution.
#' @export
#' @rdname distribution_plot
beta_plot <- function(shape1, shape2,
					  cv = .5,
					  tails = TRUE,
					  limits = c(0, 1),
					  color = 'steelblue') {
	distribution_plot(dist = 'beta',
					  shape1 = shape1,
					  shape2 = shape2,
					  tails = ifelse(tails, 'greater', 'no'),
					  cv = c(0, cv),
					  limits = limits,
					  color = color)
}

#' Plot a chi-squared distribution
#'
#' @param df degrees of freedom.
#' @seealso [stats::dchisq()]
#' @export
#' @rdname distribution_plot
chisquare_plot <- function(df = 2,
					   cv = 2,
					   tails = 'greater',
					   limits = c(0, 10),
					   color = 'steelblue') {
	if(tails != 'greater') { cv = c(0, cv) }
	distribution_plot(dist = 'chisq',
					  df = df,
					  cv = cv,
					  tails = tails,
					  limits = limits,
					  color = color)
}

#' Plot a F distribution.
#'
#' @param df1 numerator degrees of freedom.
#' @param df2 denominator degrees of freedom.
#' @seealso [stats::df()]
#' @export
#' @rdname distribution_plot
F_plot <- function(df1 = 1, df2 = 5,
				   cv = 2,
				   tails = 'greater',
				   limits = c(0, 10),
				   color = 'steelblue') {
	if(tails != 'greater') { cv = c(0, cv) }
	distribution_plot(dist = 'f',
					  df1 = df1,
					  df2 = df2,
					  cv = cv,
					  tails = tails,
					  limits = limits,
					  color = color)
}

#' Plot a student t distribution.
#'
#' @param df degrees of freedom.
#' @export
#' @rdname distribution_plot
t_plot <- function(df = 10,
				   cv = c(-1, 1),
				   tails = 'no',
				   limits = c(mean - 3 * sd, mean + 3 * sd),
				   color = 'steelblue') {
	distribution_plot(dist = 't',
					  df = df,
					  cv = cv,
					  tails = tails,
					  limits = limits,
					  color = color)
}

#' Plot a normal distribution.
#'
#' Plots a normal distribution with the area within the given cv shaded.
#' The probability (i.e. area) under the normal curve for the given cv
#' is also given.
#'
#' @param mean mean of the distribution.
#' @param sd standard deviation of the distribution.
#' @export
#' @examples
#' 	normal_plot()
#' 	normal_plot(cv=c(-2,2))
#' 	normal_plot(cv=c(-3,3))
#' 	normal_plot(cv=1.96, tails='greater')
#' @rdname distribution_plot
normal_plot <- function(mean = 0,
						sd = 1,
						cv = c(-1.96, 1.96),
						tails = 'no',
						limits = c(mean - 3 * sd, mean + 3 * sd),
						color = 'steelblue') {
	distribution_plot(dist = 'norm',
					  mean = mean,
					  sd = sd,
					  cv = cv,
					  tails = tails,
					  limits = limits,
					  color = color)
}
