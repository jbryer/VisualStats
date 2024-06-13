#' Analysis of Variance (ANOVA) Graphic
#'
#' This is a \code{ggplot2} adaptation of Pruzek and Helmreich's \code{granova::granova.1w}
#' function. This also provides parameters to customize which features are
#' included on the plot.
#'
#' @references Pruzek & Helmreich (2010). [Elemental Graphics for Analysis of Variance using the R Package granova](http://moderngraphics11.pbworks.com/f/ElementalGraphics4ANOVA.RP+JH.pdf)
#' @return a ggplot2 expression.
#' @param Y the dependent variable.
#' @param group the independent, or grouping, variable.
#' @param plot_datapoints whether to plot the individual observations.
#' @param plot_group_means whether to plot the group means.
#' @param plot_boxplot whether to plot the box plots.
#' @param plot_group_variances whether to plot the group variances.
#' @param plot_group_sd whether to plot the group standard deviations.
#' @param plot_ms_within whether to plot the mean square within.
#' @param plot_ms_between whether to plot the mean square between.
#' @param plot_between_group_variances whether to plot the between group variances.
#' @param plot_unit_line whether to plot the unit line (i.e. y = x).
#' @param plot_grand_mean whether to plot the grand mean.
#' @param plot_sd_line whether to plot the standard deviation.
#' @param plot_pooled_sd whether to plot the pooled standard deviation.
#' @param xlab label for the x-axis.
#' @param ylab label for the y-axis.
#' @param grand_mean_col color for the grand mean.
#' @param sd_line_col color for the standard deviation line.
#' @param pooled_sd_col color for the pooled standard deviation.
#' @param ms_within_col color for the mean square within.
#' @param ms_between_col color for the mean square between.
#' @param box_width width of the box express as a percentage of the width of the x-axis.
#' @param box_color color of the box plots.
#' @param plot_group_labels whether to plot the group labels on the figure.
#' @param ... currently unused.
#' @examples
#' data(hand_washing)
#' anova_vis(hand_washing$Bacterial_Counts, hand_washing$Method)
#' @export
#' @importFrom tidyr unnest_wider
#' @import dplyr
#' @importFrom ggbeeswarm geom_beeswarm
anova_vis <- function(Y,
					  group,
					  plot_datapoints = TRUE,
					  plot_group_means = TRUE,
					  plot_boxplot = FALSE,
					  plot_group_variances = TRUE,
					  plot_group_sd = TRUE,
					  plot_ms_within = TRUE,
					  plot_ms_between = TRUE,
					  plot_between_group_variances = FALSE,
					  plot_unit_line = TRUE,
					  plot_grand_mean = TRUE,
					  plot_sd_line = FALSE,
					  plot_pooled_sd = FALSE,
					  xlab = 'Deviation Contrast',
					  ylab = 'Dependent Variable',
					  grand_mean_col = 'blue',
					  sd_line_col = 'maroon', # Grand (overall) Standard Deviation
					  pooled_sd_col = 'steelblue3', # Pooled Standard Deviation
					  ms_within_col = '#fdc086',
					  ms_between_col = '#7fc97f',
					  box_width = .04,
					  box_color = 'grey50',
					  plot_group_labels = FALSE,
					  ...
) {
	df <- data.frame(Value = Y,
					 Group = group,
					 stringsAsFactors = FALSE)

	desc <- describe_by(df$Value, group = df$Group, mat = TRUE, skew = TRUE)
	names(desc)[2] <- 'Group'
	desc$Var <- desc$sd^2

	grand_mean_val <- mean(df$Value) # Weighted mean
	grand_sd <- sd(df$Value) # Weighted SD
	# grand_mean_val <- mean(desc$mean) # Unweighted mean
	# grand_var <- var(df$Value) # Weighted variance
	# grand_var <- mean(desc$Var) # Unweighted variance

	desc$contrast <- (desc$mean - grand_mean_val)

	df <- merge(df, desc[,c('Group', 'contrast', 'mean')],
				by = 'Group', all.x = TRUE)

	k <- length(unique(df$Group))
	n <- nrow(df)

	pooled_var <- mean(desc$Var)

	ss_total <- sum((df$Value - grand_mean_val)^2)
	df_between <- k - 1
	ss_between <- sum(desc$n * (desc$mean - grand_mean_val)^2)
	MS_between <- ss_between / df_between

	df_within <- n - k
	ss_within <- ss_total - ss_between
	MS_within <- ss_within / df_within

	F_stat <- MS_between / MS_within

	p <- 1 - pf(F_stat, df_between, df_within)

	slope <- (desc[1,]$mean - desc[2,]$mean) / (desc[1,]$contrast - desc[2,]$contrast)
	intercept <- desc[1,]$mean - slope * desc[1,]$contrast

	df_rect <- data.frame(
		`Mean Square` = c('Between', 'Within'),
		xmin = c(-1 * sqrt(MS_between) / 2,
				 -1 * sqrt(MS_within) / 2),
		xmax = c(     sqrt(MS_between) / 2,
					  sqrt(MS_within) / 2),
		ymin = c(grand_mean_val - 1 * sqrt(MS_between) / 2,
				 grand_mean_val - 1 * sqrt(MS_within) / 2),
		ymax = c(grand_mean_val +     sqrt(MS_between) / 2,
				 grand_mean_val +     sqrt(MS_within) / 2) )

	df_rect_within <- df %>%
		mutate(square = (Value - mean)^2) %>%
		group_by(Group) %>%
		summarize(contrast = mean(Value) - grand_mean_val,
				  mean = mean(Value),
				  MS = sum(square) / (n() - 1)) %>%
		mutate(xmin = contrast - sqrt(MS) / 2,
			   xmax = contrast + sqrt(MS) / 2,
			   ymin = mean - sqrt(MS) / 2,
			   ymax = mean + sqrt(MS) / 2)

	df_subscript <- paste0(df_between, ', ', df_within)
	title <- bquote(F[.(df_subscript)] == .(prettyNum(F_stat, digits = 3)) ~ '; p' ~ .(ifelse(p < 0.01, ' < 0.01', paste0(' = ', prettyNum(p, digits = 3)))))

	miny <- min(grand_mean_val - sqrt(MS_between) / 2,
				grand_mean_val - sqrt(MS_within) / 2,
				df$Value)
	maxy <- max(grand_mean_val + sqrt(MS_between) / 2,
				grand_mean_val + sqrt(MS_within) / 2,
				df$Value)

	ylim <- c(0.95 * miny, 1.05 * maxy)
	xlim <- c(-1 * diff(range(ylim)) / 2,
			  diff(range(ylim)) / 2)

	box_width = diff(xlim) * box_width

	p <- ggplot()

	if(plot_boxplot) {
		df_boxplot <- df %>%
			group_by(Group) %>%
			summarise(boxplot = list( setNames(boxplot.stats(Value)$stats,
							c('lower_whisker','lower_hinge','median','upper_hinge','upper_whisker') ) ) ) %>%
			unnest_wider(boxplot) %>%
			merge(desc[,c('Group', 'contrast')], by = 'Group')
		p <- p + geom_rect(data = df_boxplot, aes(xmin = contrast - box_width / 2,
											 xmax = contrast + box_width / 2,
											 ymin = lower_hinge,
											 ymax = upper_hinge),
					  color = box_color,
					  alpha = 0.0
					  ) +
			geom_segment(data = df_boxplot, aes(x = contrast - box_width / 2,
												xend = contrast + box_width / 2,
												y = median,
												yend = median),
						 color = box_color) +
			geom_segment(data = df_boxplot, aes(x = contrast,
												xend = contrast,
												y = lower_hinge,
												yend = lower_whisker),
						 color = box_color) +
			geom_segment(data = df_boxplot, aes(x = contrast,
												xend = contrast,
												y = upper_hinge,
												yend = upper_whisker),
						 color = box_color)
	}

	if(plot_between_group_variances) {
		p <- p + geom_rect(data = desc, aes(xmin = contrast, xmax = 0,
										ymin = mean, ymax = mean(df$Value),
										color = Group, fill = Group),
					   alpha = 0.05, linetype = 4)
	}

	if(plot_group_variances) {
		p <- p + geom_rect(data = df_rect_within,
						   aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = Group, color = Group),
						   alpha = 0.05, linetype = 2)
	}
	if(plot_group_sd) {
		p <- p + geom_segment(data = desc,
							  aes(x = contrast, xend = contrast, y = mean - sd / 2, yend = mean + sd / 2),
							  alpha = 0.6)
	}

	if(plot_ms_within) {
		p <- p + geom_rect(data = df_rect[2,],
						   aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, group = Mean.Square),
						   alpha = 0.4, fill = ms_within_col)

	}

	if(plot_ms_between) {
		p <- p + geom_rect(data = df_rect[1,],
						   aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, group = Mean.Square),
						   alpha = 0.1, fill = ms_between_col)
	}

	if(plot_unit_line) {
		p <- p + geom_abline(slope = slope, intercept = intercept, color = 'grey70')
	}

	if(plot_grand_mean) {
		p <- p +
			geom_hline(yintercept = mean(df$Value), alpha = 0.5, linetype = 2, size = 1) +
			geom_vline(xintercept = 0, alpha = 0.5, linetype = 2, size = 1) +
			geom_point(aes(x = 0, y = grand_mean_val), color = grand_mean_col, size = 3)
	}

	if(plot_sd_line) {
		p <- p +
			geom_hline(yintercept = c(grand_mean_val - grand_sd / 2,
									  grand_mean_val + grand_sd / 2),
					   linetype = 5, color = sd_line_col, alpha = 0.5)
	}

	if(plot_pooled_sd) {
		p <- p +
			geom_hline(yintercept = c(df_rect[2,]$ymin,
									  df_rect[2,]$ymax),
					   linetype = 5,
					   color = pooled_sd_col,
					   alpha = 0.5)
	}

	if(plot_datapoints) {
		p <- p + geom_beeswarm(data = df, aes(x = contrast, y = Value,
											  group = Group, color = Group, fill = Group),
							   alpha = 0.2, size = 2)
	}

	if(plot_group_means) {
		p <- p + geom_point(data = desc, aes(x = contrast, y = mean, color = Group), size = 3)
	}

	if(plot_group_labels) {
		p <- p + geom_text(data = desc, aes(label = Group, x = contrast, y = min(df$Value)),
				  angle = 90, hjust = 0, vjust = -0.8)
	}

	p <- p + ggtitle(title) +
		xlim(xlim) + ylim(ylim) +
		xlab(xlab) +
		ylab(ylab) +
		coord_equal() +
		theme_vs()

	return(p)
}

if(FALSE) { #TODO: move to testthat
	library(VisualStats)
	library(tidyverse)
	data('hand_washing', envir = environment())
	anova_vis(Y = hand_washing$Bacterial_Counts,
			  group = hand_washing$Method,
			  plot_boxplot = FALSE,
			  plot_group_variances = TRUE,
			  plot_group_sd = FALSE,
			  plot_ms_within = TRUE,
			  plot_ms_between = TRUE,
			  plot_unit_line = TRUE,
			  plot_grand_mean = TRUE,
			  plot_sd_line = FALSE,
			  plot_pooled_sd = FALSE,
			  plot_group_labels = FALSE,
			  ylab = 'Bacterial Count'
	)

	data('iris', envir = environment())
	anova_vis(Y = iris$Sepal.Length,
			  group = iris$Species,
			  plot_boxplot = FALSE,
			  plot_group_variances = TRUE,
			  plot_group_sd = FALSE,
			  plot_ms_within = TRUE,
			  plot_ms_between = TRUE,
			  plot_unit_line = TRUE,
			  plot_grand_mean = TRUE,
			  plot_sd_line = FALSE,
			  plot_pooled_sd = FALSE,
			  plot_group_labels = FALSE,
			  ylab = 'Sepal Length')

	data('penguins', package = 'palmerpenguins', envir = environment())
	penguins <- penguins[complete.cases(penguins),]
	set.seed(2112)
	penguins <- penguins[sample(nrow(penguins), size = .1 * nrow(penguins)),]
	anova_vis(Y = penguins$flipper_length_mm,
			  group = penguins$species,
			  plot_boxplot = FALSE,
			  plot_group_variances = TRUE,
			  plot_group_sd = FALSE,
			  plot_ms_within = TRUE,
			  plot_ms_between = TRUE,
			  plot_unit_line = FALSE,
			  plot_grand_mean = TRUE,
			  plot_sd_line = FALSE,
			  plot_pooled_sd = FALSE,
			  plot_group_labels = FALSE,
			  ylab = 'Flipper Length')

}
