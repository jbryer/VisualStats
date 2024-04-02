#' Loess Visualization
#'
#' @export
#' @examples
#' data("faithful")
#' loess_vis(formula = eruptions ~ waiting, data = faithful)
loess_vis <- function(formula,
					  data,
					  center = median(data[,all.vars(formula)[2],drop=TRUE]),
					  degree = 2,
					  span = 0.3,
					  draw_loess = FALSE,
					  show_loess = TRUE,
					  xlab = all.vars(formula)[2],
					  ylab = all.vars(formula)[1],
					  ...) {
	df <- data %>% rename(y = all.vars(formula)[1],
						  x = all.vars(formula)[2])
	loess.out <- loess(y ~ x,
					   data = df,
					   degree = degree,
					   span = span)
	df <- df %>% mutate(fitted = fitted(loess.out))

	df.points <- df %>%
		mutate(dist = abs(x - center)) %>%
		filter(rank(dist) / n() <= span) %>%
		mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3)

	local_formula <- 'y ~ x'
	if(degree > 1) {
		for(i in 2:degree) {
			local_formula <- paste0(local_formula, ' + I(x^', i, ')')
		}
	}

	p <- ggplot(df.points, aes(x = x, y = y)) +
		geom_vline(xintercept = center, linetype = 2) +
		geom_point(data = df, alpha = 0.5, shape = 1) +
		geom_point(aes(color = weight)) +
		geom_smooth(method = 'lm',
					formula = as.formula(local_formula),
					aes(weight = weight),
					se = FALSE, color = '#43CD80', size = 0.5) +
		geom_point(x = center,
				   y = predict(loess.out, newdata = data.frame(x = center)),
				   color = 'orange', size = 4) +
		#            scale_color_gradient2(low = '#ece7f2', high = '#2b8cbe') +
		scale_color_gradient2(low = '#ece7f2', high = '#2E8B57') +
		ylim(c(min(df$y) - 0.05 * diff(range(df$y)),
			   max(df$y) + 0.05 * diff(range(df$y)))) +
		xlab(xlab) +
		ylab(ylab) +
		theme_vs() +
		theme(text = element_text(size = 16))

	if(draw_loess) {
		p <- p+ geom_line(data = df[df$x <= center,], aes(y = fitted),
						  color = 'orange', size = 1)
	}

	if(show_loess) {
		p <- p + geom_line(data = df, aes(y = fitted),
						   color = 'grey50', size = 0.5)
	}

	return(p)
}
