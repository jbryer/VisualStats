#' Independent Sample Plot
#'
#' This is adapted from the plots introduced by Gardner and Altman (1986) for displaying overlapping
#' confidence intervals for independent sample tests.
#'
#' @param df a data.frame containing the data.
#' @param group_col the name of the column in `df` containing the group membership.
#' @param value_col the name of the column in `df` containing the values.
#' @param colors vector of length three for the group colors. The last value is the color for the
#'        differences.
#' @param plot_se whether to plot the standard error distributions (to the right of the points.
#' @param plot_distribuition whether to plot the raw distributions (to the left of the points).
#' @param plot_zero_difference whether to plot a horizontal line representing a zero difference.
#' @param plot_ci whether to plot confidence intervals as vertical bars.
#' @param plot_means whether to plot the group means.
#' @param plot_points whether to plot the raw observations.
#' @return a ggplot2 expression.
#' @references Gardner, M.J. & Altman, D.G. (1986). Confidence intervals rather than P values:
#' estimation rather than hypothesis testing. *British Medical Journal, 292*(6522) 746-750.
#' doi:10.1136/bmj.292.6522.746
#' @export
#' @importFrom dplyr arrange mutate
#' @importFrom reshape2 melt
#' @importFrom ggdist geom_slab geom_dots stat_slabinterval
#' @import ggplot2
#' @importFrom stats t.test qt dt
#' @examples
#' set.seed(42)
#' # Can adjust these parameters
#' n <- 20
#' mean1 <- 10
#' sd1 <- 2
#' mean2 <- 14
#' sd2 <- 2
#' df <- data.frame(
#' 	group = c(rep('Group A', n / 2),
#' 			  rep('Group B', n / 2)),
#' 	value = c(rnorm(n = (n / 2), mean = mean1, sd = sd1),
#' 			  rnorm(n = (n / 2), mean = mean2, sd = sd2))
#' )
#'
#' independent_sample_vis(df)
independent_sample_vis <- function(
	df,
	group_col = names(df)[1], # TODO: can probably write a function to better guess the default values
	value_col = names(df)[2],
	colors = c('#66c2a5', '#8da0cb', '#fc8d62'),
	plot_se = TRUE,
	plot_distribuition = TRUE,
	plot_zero_difference = TRUE,
	plot_ci = TRUE,
	plot_means = TRUE,
	plot_points = ifelse(nrow(df) < 60, TRUE, FALSE)
) {
	names(df)[names(df) == group_col] <- 'group'
	names(df)[names(df) == value_col] <- 'value'

	if(length(unique(df$group)) != 2) {
		stop('There can only be two groups')
	}

	n <- table(df$group) |> unname()

	t_out <- stats::t.test(value ~ group, data = df)

	tab <- describe_by(df$value, group = df$group) |>
		dplyr::rename(group = group1) |>
		dplyr::arrange(mean)

	cv <- qt(0.025, df = nrow(df) - 2) # Critical Value for differences

	tab$cv <- abs(qt(0.025, df = tab$n - 1))

	x <- seq(min(df$value), max(df$value), by = 0.01)

	t_dist <- data.frame(
		x = x,
		A = stats::dt(x - tab[1,]$mean, df = tab[1,]$n - 1),
		B = stats::dt(x - tab[2,]$mean, df = tab[2,]$n - 1),
		Difference = stats::dt(x - tab[2,]$mean, df = n - 2)
	) |>
		reshape2::melt(id.vars = 'x') |>
		dplyr::rename(group = variable) |>
		dplyr::mutate(group = as.character(group)) |>
		dplyr::mutate(group = replace(group, group == 'A', tab[1,]$group),
					  group = replace(group, group == 'B', tab[2,]$group))

	names(colors) <- c(tab$group, 'Difference')
	raw_dist <- ggdist::stat_slabinterval # TODO: make paramter
	# raw_dist <- ggdist::stat_dotsinterval
	# raw_dist <- ggdist::stat_histinterval


	p <- ggplot(t_dist, aes(x = group, y = x, color = group))

	if(plot_se) {
		p <- p + ggdist::geom_slab(aes(thickness = value, fill = group), alpha = 0.5)
	}

	if(plot_distribuition) {
		p <- p + raw_dist(data = df, aes(y = value, x = group, fill = group),
						  side = 'left', alpha = 0.2)
	}
	if(plot_zero_difference) {
		p <- p +
			geom_hline(yintercept = tab[1,]$mean, linewidth = 1.5) +
			annotate("label", x = 'Difference', y = tab[1,]$mean, label = 'Zero Difference',
					 color = 'black', fill = 'white', hjust = 0, vjust = 1.5)

	}
	# Extreme confidence intervals
	# if(plot_ci) {
	# 	p <- p +
	# 		geom_hline(yintercept = (tab[1,]$mean - cv * tab[1,]$se), linetype = 2, color = colors[1]) +
	# 		geom_hline(yintercept = (tab[2,]$mean + cv * tab[2,]$se), linetype = 2, color = colors[2])
	# }
	if(plot_ci) {
		p <- p +
			geom_segment(data = tab, aes(x = group, xend = group, y = mean - se, yend = mean + se),
						 linewidth = 6, alpha = 0.75) +
			geom_segment(data = tab, aes(x = group, xend = group, y = mean - cv * se, yend = mean + cv * se),
						 linewidth = 3, alpha = 0.75)

	}
	# p + geom_segment(x = 'Difference', xend = 'Difference',
	# 				 y = tab[1,]$mean - t_out$conf.int[1], yend = tab[1,]$mean - t_out$conf.int[2],
	# 				 linewidth = 3, alpha = 0.5)

	if(plot_points) {
		p <- p +
			geom_point(data = df, aes(x = group, y = value),
					   shape = 21, stroke = 0.5, alpha = 0.75, color = 'black')
	}

	if(plot_means) {
		p <- p +
			geom_point(data = tab, aes(x = group, y = mean, color = group), size = 4) +
			geom_point(data = tab, aes(x = group, y = mean),
					   color = 'black', size = 4, shape = 21, stroke = 2) +
			geom_point(x = 'Difference', y = tab[1,]$mean + diff(t_out$estimate),
					   color = 'black', size = 4, shape = 21, stroke = 2) +
			annotate("label", x = 'Difference', y = tab[1,]$mean + diff(tab$mean),
					 color = colors[3], fill = 'white',
					 label = paste0('', print_num(diff(t_out$estimate))),
					 hjust = -0.5) +
			annotate("label", x = tab[1,]$group, y = tab[1,]$mean,
					 color = colors[1], fill = 'white',
					 label = paste0('', print_num(tab[1,]$mean)),
					 hjust = -0.5) +
			annotate("label", x = tab[2,]$group, y = tab[2,]$mean,
					 color = colors[2], fill = 'white',
					 label = paste0('', print_num(tab[2,]$mean)),
					 hjust = -0.5)
	}

	p <- p +
		scale_y_continuous(name = 'Value',
						   sec.axis = sec_axis(~. - mean(tab[1,]$mean), name = "Difference")) +
		scale_fill_manual(values = colors) +
		scale_color_manual(values = colors) +
		scale_x_discrete(limits = c(tab$group, 'Difference'),
						 labels = c(tab$group, paste0('Difference\n(',
						 							 tab$group[2], ' - ', tab$group[1], ')'))) +
		theme_vs() +
		theme(legend.position = 'none') +
		ggtitle('Confidence Interval Overlap',
				subtitle = paste0(
					'Mean difference = ', print_num(diff(tab$mean)), '; ',
					't = ', print_num(t_out$statistic), '; ',
					'p = ', plot_p_value(t_out$p.value))) +
		xlab('') + ylab('Value')

	return(p)
}

#' Internal function for printing numbers.
#'
#' @param x the numeric value.
#' @param big.mark character; if not empty used as mark between every big.interval decimals before
#'        (hence big) the decimal point.
#' @param scientific logical, whether to use scientific notation.
#' @param digits the desired number of digits after the decimal point.
#' @param ... other parameters passed to [prettyNum()]
print_num <- function(x, big.mark = ',', scientific = FALSE, digits = 3, ...) {
	prettyNum(x, big.mark = big.mark, scientific = scientific, digits = digits, ...)
}

#' Internal function for printing p-values.
#'
#' If the p-value is less than the threshold, then p < X will be printed, otherwise
#' `print_num()` will be called.
#'
#' @param p the p-value.
#' @param threshold what threshold should the p-value be printed as p < threshold.
#' @param ... other parameters passed to [print_num()].
#' @return a character representation of the p-value.
plot_p_value <- function(p, threshold = 0.01, ...) {
	if(p < threshold) {

	} else {
		print_num(p, ...)
	}
}

if(FALSE) { # For testing
	library(ggplot2)
	library(VisualStats)
	set.seed(42)
	n <- 20
	mean1 <- 10
	sd1 <- 2
	mean2 <- 14
	sd2 <- 2
	df <- data.frame(
		group = c(rep('Group A', n / 2),
				  rep('Group B', n / 2)),
		value = c(rnorm(n = (n / 2), mean = mean1, sd = sd1),
				  rnorm(n = (n / 2), mean = mean2, sd = sd2))
	)
	independent_sample_vis(df)

	independent_sample_vis(
		df,
		plot_se = TRUE,
		plot_distribuition = TRUE,
		plot_zero_difference = TRUE,
		plot_ci = TRUE,
		plot_means = TRUE,
		plot_points = TRUE
	)

}
