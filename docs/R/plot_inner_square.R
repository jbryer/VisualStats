#' Plots a circle within a circle.
#'
#' @export
plot_inner_square <- function(percent = 0.5, inner = c('center', 'bottom')) {
	width1 <- 1
	area1 <- width1^2
	area2 <- percent * area1
	width2 <- sqrt(area2)
	xmin <- ymin <- 0
	if(inner[1] == 'center') {
		xmin <- ymin <- (width1 / 2) - (width2 / 2)
	}
	ggplot() +
		geom_rect(aes(xmin = 0, ymin = 0, xmax = width1, ymax = width1),
				  fill = 'grey80') +
		geom_rect(aes(xmin = xmin,
					  ymin = ymin,
					  xmax = xmin + width2,
					  ymax = ymin + width2),
				  fill = 'grey20') +
		ylim(c(0,1)) + xlim(c(0,1)) +
		coord_equal() +
		theme_void()
}
