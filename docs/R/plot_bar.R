#' Plots a bar.
#'
#' @export
plot_bar <- function(percent = 0.5) {
	ggplot() +
		geom_rect(aes(xmin = 0, ymin = 0, xmax = 1, ymax = 1),
				  fill = 'grey80') +
		geom_rect(aes(xmin = 0,
					  ymin = 0,
					  xmax = 1,
					  ymax = percent),
				  fill = 'grey20') +
		ylim(c(0,1)) + xlim(c(0,1)) +
		coord_equal() +
		theme_void()
}
