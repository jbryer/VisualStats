#' Plot the four distributions functions.
#'
#' When working with distributions in R, each distribution has four functions, namely:
#'
#' * `dXXX` - density function.
#' * `rXXX` - generate random number from this distribution.
#' * `pXXX` - returns the area to the left of the given value.
#' * `qXXX` - returns the quantile for the given value.
#'
#' Where `XXX` is the distribution name (e.g. `norm`, `binom`, `t`, etc.).
#'
#' @param dist name of the distribution function to plot (e.g. `norm`, `t`).
#' @param xvals inputs to `dist` function.
#' @param xmin minimum x-axis value to plot.
#' @param xmax maximum x-axis value to plot.
#' @param args parameters to pass to `dist`.
#' @param palette colors to use for the four subplots. Must be a named vector with d, r, p, and q.
#' @export
#' @examples
#' # Normal distribution with mean 100 and standard deviation 15
#' plot_distributions(dist = 'norm',
#'                    xvals = c(80, 90, 105),
#'                    xmin = 55,
#'                    xmax = 145,
#'                    args = list(mean = 100, sd = 15))
#'
#' # Binomial distribution
#' plot_distributions(dist = 'binom',
#' 				      xvals = c(1, 3),
#' 				      xmin = 0,
#' 				      xmax = 10,
#' 				      args = list(size = 10, prob = 0.35))
plot_distributions <- function(
		dist,
		xvals,
		xmin, xmax,
		args = list(),
		palette = c(d = '#1b9e77', r = '#d95f02', p = '#7570b3', q = '#e7298a')
) {
	functions <- c(d = get(paste0('d', dist)),
				   r = get(paste0('r', dist)),
				   p = get(paste0('p', dist)),
				   q = get(paste0('q', dist)))

	suppressWarnings({
		args$x <- seq(floor(xmin) + 0.5, ceiling(xmax) - 0.5, by = 1)
		out <- do.call(
			what = functions[['d']],
			args = args
		)
		args$x <- NULL
		is_discrete <- all(out == 0)
		args$x <- NULL
	})

	df <- tibble(
		x = xvals,
		d = sapply(xvals, FUN = function(x) { args$x <- x; do.call(functions[['d']], args = args) }),
		p = sapply(xvals, FUN = function(x) { args$q <- x; do.call(functions[['p']], args = args) })
	)

	args_str <- ifelse(length(args) > 0,
					   paste0(names(args), ' = ', args, collapse = ', '),
					   '')

	d_plot <- ggplot(df, aes(x = x, y = d))
	r_plot <- ggplot(df, aes(x = x, y = d))
	p_plot <- ggplot(df, aes(x = x, y = d))
	q_plot <- ggplot(df, aes(x = x, y = d))

	if(is_discrete) {
		df_discrete <- data.frame(x = seq(floor(xmin), ceiling(xmax), by = 1))
		args$x <- df_discrete$x
		df_discrete$d <- do.call(functions[['d']], args = args)
		df_discrete$cumsum <- cumsum(df_discrete$d)

		bar_width <- 0.3
		bar_color <- 'grey80'

		d_plot <- d_plot +
			scale_x_continuous(breaks = seq(xmin, xmax, by = 1)) +
			geom_path(data = df_discrete, aes(x = x, y = d),
					  color = bar_color,
					  size = 0.5) +
			geom_bar(data = df_discrete, aes(x = x, y = d),
					 fill = bar_color,
					 width = bar_width,
					 stat = 'identity')
		r_plot <- r_plot +
			scale_x_continuous(breaks = seq(xmin, xmax, by = 1)) +
			geom_path(data = df_discrete, aes(x = x, y = d),
					  color = bar_color,
					  size = 0.5) +
			geom_bar(data = df_discrete, aes(x = x, y = d),
					 fill = bar_color,
					 width = bar_width,
					 stat = 'identity')
		p_plot <- p_plot +
			scale_x_continuous(breaks = seq(xmin, xmax, by = 1)) +
			geom_bar(data = df_discrete, aes(x = x, y = cumsum),
					 stat = 'identity',
					 fill = bar_color,
					 width = bar_width) +
			# geom_point(data = df_discrete, aes(x = x, y = cumsum),
			# 		   color = bar_color,
			# 		   size = 1) +
			geom_path(data = df_discrete, aes(x = x, y = cumsum),
					  color = bar_color,
					  size = 0.5)
		q_plot <- q_plot +
			scale_x_continuous(breaks = seq(xmin, xmax, by = 1)) +
			geom_bar(data = df_discrete, aes(x = x, y = cumsum),
					 stat = 'identity',
					 fill = bar_color,
					 width = bar_width) +
			# geom_point(data = df_discrete, aes(x = x, y = cumsum),
			# 		   color = bar_color,
			# 		   size = 1) +
			geom_path(data = df_discrete, aes(x = x, y = cumsum),
					  color = bar_color,
					  size = 0.5)
	} else {
		d_plot <- d_plot +
			xlim(xmin, xmax) +
			geom_function(fun = functions[['d']], args = args)
		r_plot <- r_plot +
			xlim(xmin, xmax) +
			geom_function(fun = functions[['d']], args = args)
		p_plot <- p_plot +
			xlim(xmin, xmax) +
			geom_function(fun = functions[['p']], args = args)
		q_plot <- q_plot +
			xlim(xmin, xmax) +
			geom_function(fun = functions[['p']], args = args)
	}

	d_plot <- d_plot +
		geom_segment(aes(x = x, xend = x, y = 0, yend = d), color = palette['d']) +
		geom_segment(aes(x = x, xend = xmin, y = d, yend = d), color = palette['d'],
					 arrow = arrow(length = unit(0.5, "cm"))) +
		geom_point(aes(x = x, y = 0), color = palette['d'], size = 2) +
		xlab('z-score / quantile') + ylab('Probability Density') +
		ggtitle(paste0('d', dist, '(', args_str, ')')) +
		theme_vs()

	r_plot <- r_plot +
		geom_segment(aes(x = x, xend = x, y = d, yend = 0), color = palette['r'],
					 arrow = arrow(length = unit(0.5, "cm"))) +
		xlab('z-score / quantile') + ylab('Probability Density') +
		ggtitle(paste0('r', dist, '(', args_str, ')')) +
		theme_vs()

	p_plot <- p_plot +
		geom_segment(aes(x = x, xend = x, y = 0, yend = p), color = palette['p']) +
		geom_segment(aes(x = x, xend = xmin, y = p, yend = p), color = palette['p'],
					 arrow = arrow(length = unit(0.5, "cm"))) +
		geom_point(aes(x = x, y = 0), color = palette['p'], size = 2) +
		xlab('z-score / quantile') + ylab('Cumulative Probability') +
		ggtitle(paste0('p', dist, '(', args_str, ')')) +
		theme_vs()

	q_plot <- q_plot +
		geom_segment(aes(x = x, xend = x, y = p, yend = 0), color = palette['q'],
					 arrow = arrow(length = unit(0.5, "cm"))) +
		geom_segment(aes(x = x, xend = xmin, y = p, yend = p), color = palette['q']) +
		geom_point(aes(x = xmin, y = p), color = palette['q'], size = 2) +
		xlab('z-score / quantile') + ylab('Cumulative Probability') +
		ggtitle(paste0('q', dist, '(', args_str, ')')) +
		theme_vs()

	plot_grid(d_plot, r_plot, p_plot, q_plot)
}

if(FALSE) {
	plot_distributions(dist = 'norm',
					   xvals = c(-1, 0, 0.5),
					   xmin = -4, xmax = 4)

	plot_distributions(dist = 'norm',
					   xvals = c(80, 90, 105),
					   xmin = 55, xmax = 145,
					   args = list(mean = 100, sd = 15))

	plot_distributions(dist = 'binom',
					   xvals = c(1, 3),
					   xmin = 0, xmax = 10,
					   args = list(size = 10, prob = 0.35))

	plot_distributions(dist = 'chisq',
					   xvals = c(1, 2, 5),
					   xmin = 0, xmax = 10,
					   args = list(df = 3))

	plot_distributions(dist = 'f',
					   xvals = c(0.5, 1, 2),
					   args = list(df1 = 3, 12),
					   xmin = 0, xmax = 10)

	plot_distributions(dist = 't',
					   xvals = c(-1, 0, 0.5),
					   xmin = -4, xmax = 4,
					   args = list(df = 5))

	plot_distributions(dist = 'binom',
					   xvals = c(1, 4),
					   xmin = 0, xmax = 10,
					   args = list(size = 10, prob = 0.35))
}
