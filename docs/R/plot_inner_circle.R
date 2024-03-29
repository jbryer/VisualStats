#' Plots a circle within a circle.
#'
#' @export
plot_inner_circle <- function(percent = 0.5, y_inner = c('center', 'bottom')) {
	r1 <- 0.5
	area1 <- pi * r1^2
	area2 <- percent * area1
	r2 <- sqrt(area2 / pi)
	y2 <- ifelse(y_inner[1] == 'center', 0.5, r2)

	ggplot() +
		ggforce::geom_circle(aes(x0 = 0.5, y0 = 0.5, r = r1),
					fill = 'grey80', color = NA) +
		ggforce::geom_circle(aes(x0 = 0.5, y0 = y2, r = r2),
					fill = 'grey20', color = NA) +
		ylim(c(0,1)) + xlim(c(0,1)) +
		coord_equal() +
		theme_void()
}
