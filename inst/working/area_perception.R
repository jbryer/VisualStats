library(VisualStats)

plot_inner_square(inner = 'middle')
plot_inner_square(inner = 'bottom')

plot_bar()

plot_inner_circle(0.25, y_inner = 'middle')
plot_inner_circle(0.25, y_inner = 'bottom')

library(base64enc)
fn <- tempfile(fileext='.png')
png(fn)
print(plot_inner_circle(0.25, y_inner = 'middle'))
dev.off()

img <- paste0('data:image/jpeg;base64,', base64enc::base64encode(fn))

percent <- 0.5
cowplot::plot_grid(
	plot_inner_square(percent, inner = 'middle'),
	plot_inner_square(percent, inner = 'bottom'),
	plot_inner_circle(percent, y_inner = 'middle'),
	plot_inner_circle(percent, y_inner = 'bottom'),
	plot_bar(percent),
	nrow = 1
)
