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
#' @examples
#' data(hand_washing)
#' anova_vis(hand_washing$Bacterial.Counts, hand_washing$Method)
#' @export
anova_vis <- function(Y,
					  group,
					  plot_group_variances = TRUE,
					  plot_group_sd = TRUE,
					  plot_ms_within = TRUE,
					  plot_ms_between = TRUE,
					  plot_unit_line = TRUE,
					  plot_grand_mean = TRUE,
					  plot_sd_line = TRUE,
					  plot_pooled_sd = TRUE,
					  xlab = 'Contrast Coefficient',
					  ylab = 'Dependent Variable',
					  grand_mean_col = 'blue',
					  sd_line_col = 'maroon', # Grand (overall) Standard Deviation
					  pooled_sd_col = 'steelblue3', # Pooled Standard Deviation
					  ms_within_col = '#fdc086',
					  ms_between_col = '#7fc97f',
					  ...
) {
	df <- data.frame(Value = Y,
					 Group = group,
					 stringsAsFactors = FALSE)

	desc <- psych::describeBy(df$Value, group = df$Group, mat = TRUE, skew = FALSE)
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

	p <- ggplot()
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
			geom_point(aes(x = 0, y = grand_mean_val), color = grand_mean_col, size = 4)
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

	xlim <- c(-1.1 * max(2 * sqrt(MS_between), diff(range(df$Value)) ) / 2,
			  1.1 * max(2 * sqrt(MS_between), diff(range(df$Value))) / 2)
	ylim <- c(1.1 * (grand_mean_val - max(2 * sqrt(MS_between), diff(range(df$Value))) / 2),
			  1.1 * (grand_mean_val + max(2 * sqrt(MS_between), diff(range(df$Value))) / 2))

	p <- p +
		geom_point(data = df, aes(x = contrast, y = Value, group = Group, color = Group),
				   alpha = 0.75, shape = 1, size = 2) +
		geom_point(data = desc, aes(x = contrast, y = mean, color = Group), size = 3) +
		geom_text(data = desc, aes(label = Group, x = contrast, y = min(df$Value)),
				  angle = 90, hjust = 0, vjust = -0.8) +
		ggtitle(title) +
		xlim(xlim) + ylim(ylim) +
		xlab(xlab) +
		ylab(ylab) +
		coord_equal() +
		theme_minimal() +
		theme(panel.grid.major = element_line(color = 'grey90', size = 0.3),
			  panel.grid.minor = element_blank(),
			  legend.position = 'bottom')

	return(p)
}
