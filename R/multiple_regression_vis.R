#' 3d plot scatter plot with regression plane
#'
#' @param y dependent varaible.
#' @param x1 independent variable.
#' @param x2 independent variable.
#' @param plot_regression whether to plot the regression plane.
#' @param interaction whether to plot the plane including the interaction effects (i.e. warped plane).
#' @param plot_residuals if `TRUE` the residuals will be plotted as line segments.
#' @param plot_slopes either `x1` or `x2` to plot the simple slopes (i.e. plus and minus one
#'        standard deviation) or `NULL` to exclude.
#' @param x1_lab label for x1-axis.
#' @param x2_lab label for x2-axis.
#' @param y_lab label for the y-axis.
#' @return a plot_ly expression.
#' @export
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom stats coef
#' @examples
#' data(depression, package = 'VisualStats')
#' multiple_regression_vis(y = depression$depression,
#'     x1 = depression$anxiety,
#'     x2 = depression$affect,
#'     y_lab = 'Depression',
#'     x1_lab = 'Anxiety',
#'     x2_lab = 'Affect',
#'     interaction = TRUE)
#'
multiple_regression_vis <- function(
		y, x1, x2,
		interaction = FALSE,
		plot_regression = TRUE,
		plot_residuals = TRUE,
		plot_slopes = NULL,
		y_lab = 'Y',
		x1_lab = 'X1',
		x2_lab = 'X2'
) {
	df <- data.frame(y = y, x1 = x1, x2 = x2)
	lm_out <- NULL
	if(interaction) {
		lm_out <- lm(y ~ x1 * x2, data = df)
	} else {
		lm_out <- lm(y ~ x1 + x2, data = df)
	}
	df$predicted <- predict(lm_out)

	coef_out <- coef(lm_out)
	x_seq <- seq(min(x1), max(x1), length.out = 25)
	y_seq <- seq(min(x2), max(x2), length.out = 25)
	z_seq <- t(outer(x_seq, y_seq, function(x, y) {
		predict(lm_out, newdata = data.frame(x1 = x, x2 = y)) }))

	plot <- plotly::plot_ly()

	if(plot_regression) {
		plot <- plot |>
			plotly::add_trace(
				x = ~x_seq,
				y = ~y_seq,
				z = ~z_seq,
				type = "surface",
				showlegend = FALSE,
				showscale = FALSE
			)
	}

	plot <- plot |>
		plotly::add_trace(
			data = df,
			x = ~x1,
			y = ~x2,
			z = ~y,
			mode = "markers",
			type = "scatter3d",
			# color = c('#000000', '#000000', '#000000'),
			marker = list(opacity = 0.7, symbol = 105, size = 3),
			showlegend = FALSE
		) |>
		plotly::layout(
			showlegend = FALSE,
			scene = list(
				xaxis = list(title = x1_lab),
				yaxis = list(title = x2_lab),
				zaxis = list(title = y_lab)
				# camera = list(eye = list(x = 2.0, y = 2.2, z = 1))
			)
		)

	if(plot_residuals) {
		for(i in 1:nrow(df)) {
			line_segments <- data.frame(
				x1 = c(df[i,]$x1, df[i,]$x1),
				x2 = c(df[i,]$x2, df[i,]$x2),
				y = c(df[i,]$y, df[i,]$predicted)
			)

			plot <- plot |>
				plotly::add_trace(
					data = line_segments,
					x = ~x1,
					y = ~x2,
					z = ~y,
					mode = "lines",
					type = "scatter3d"
				)
		}
	}

	if(!is.null(plot_slopes)) {
		if(!plot_slopes %in% c('x1', 'x2')) {
			stop('plot_slopes parameter must be either x1 or x2')
		}
		# @param dist distance from the mean expressed in standard deviation units
		# @param direction which variable is held constant.
		add_slope <- function(plot, dist = 1, direction = 'x1') {
			constant <- ifelse(direction == 'x1', 'x2', 'x1')
			x1_mean <- mean(df[,direction])
			x1_sd <- sd(df[,direction])
			min_x2 <- min(df[,constant])
			max_x2 <- max(df[,constant])
			simple_slopes <- data.frame(
				c(x1_mean + dist * x1_sd, x1_mean + dist * x1_sd),
				c(min_x2, max_x2)
			)
			names(simple_slopes) <- c(direction, constant)
			simple_slopes$y <- predict(lm_out, newdata = simple_slopes)
			plot |>
				plotly::add_trace(
					data = simple_slopes,
					x = ~x1,
					y = ~x2,
					z = ~y,
					mode = "lines",
					type = "scatter3d",
					line = list(width = 10, color = 'black')
				) |>
				plotly::add_trace(
					data = simple_slopes,
					x = ~x1,
					y = ~x2,
					z = ~y,
					mode = "markers",
					type = "scatter3d",
					marker = list(size = 5, color = 'black')
				)
		}
		plot <- plot |> add_slope(dist = 1, direction = plot_slopes)
		plot <- plot |> add_slope(dist = -1, direction = plot_slopes)
	}
	return(plot)
}

if(FALSE) { # For debugging
	# data(depression, package = 'VisualStats')
	depression <- VisualStats::depression
	multiple_regression_vis(
		y = depression$depression,
		x1 = depression$anxiety,
		x2 = depression$affect,
		y_lab = 'Depression',
		x1_lab = 'Anxiety',
		x2_lab = 'Affect',
		interaction = TRUE,
		plot_residuals = TRUE,
		plot_slopes = 'x2'
	)
	y = depression$depression
	x1 = depression$anxiety
	x2 = depression$affect
	y_lab = 'Depression'
	x1_lab = 'Anxiety'
	x2_lab = 'Affect'
	interaction = TRUE
}
