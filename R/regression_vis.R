#' Correlation and cross product visualization
#'
#' @param df data.frame with the data to plot.
#' @param x_var the name of the variable for the x-axis.
#' @param y_var the name of the variable fro teh y-axis.
#' @param plot_x_mean plot a line for the mean of x.
#' @param plot_y_mean plot a line for the mean of y.
#' @param plot_positive_cross_products plot all the cross products in the 1st and 3rd quadrants.
#' @param plot_negative_cross_products plot all the cross products in the 2nd and 4th quandrants.
#' @param plot_x_deviations either a logical or numerical vector for the rows in `df`
#'        to plot the x deviations.
#' @param plot_y_deviations either a logical or numerical vector for the rows in `df`
#'        to plot the y deviations.
#' @param plot_cross_products either a logical or numerical vector for the rows in `df`
#'        to plot the cross products.
#' @param plot_regression whether to plot the linear regression line.
#' @param cross_product_fill the fill for the cross products plotted by `plot_cross_products`.
#' @param positive_cross_product_fill the fill for the positive cross products.
#' @param negative_cross_product_fill the fill for the negative cross products.
#' @param x_deviation_color the color of the line for x deviations.
#' @param y_deviation_color the color of the line for y deviations.
#' @param deviation_size of the line when plotting deviations.
#' @param cross_product_color the color of the square for cross products.
#' @param positive_cross_product_color the color of the square for positive cross products.
#' @param negative_cross_product_color the color of the square for negative cross products.
#' @param cross_product_alpha the alpha level (transparency) for the cross product fill.
#' @param regression_line_color color for the regression line.
#' @param plot_residuals whether to plot the residuals.
#' @param plot_residuals_squared whether to plot the squared residuals.
#' @param residuals_squared_color color for the residuals.
#' @param residuals_squared_fill fill color for the squared residuals.
#' @param residuals_squared_alpha alpha (transparency) level for the squared residuals.
#' @param residual_color color for the residuals.
#' @param residual_size size of the residuals.
#' @return a ggplot2 expression.
#' @import ggplot2
#' @export
#' @examples
#' df <- mtcars[,c('wt', 'mpg')]
#' cross_products <- abs(df[,1] * df[,2])
#' cross_products == max(cross_products) # Find the largest cross product
#'
#' # Scatter plot with arrows showing the largest cross product
#' regression_vis(df,
#' 				plot_x_mean = TRUE,
#' 				plot_y_mean = TRUE,
#' 				plot_positive_cross_products = FALSE,
#' 				plot_negative_cross_products = FALSE,
#' 				plot_x_deviations = cross_products == max(cross_products),
#' 				plot_y_deviations = cross_products == max(cross_products))
#'
#' # Scatter plot with the largest cross product as a rectangl
#' regression_vis(df,
#' 				plot_x_mean = TRUE,
#' 				plot_y_mean = TRUE,
#' 				plot_positive_cross_products = FALSE,
#' 				plot_negative_cross_products = FALSE,
#' 				plot_cross_products = cross_products == max(cross_products),
#' 				cross_product_alpha = 0.5)
#'
#' # Scatter plot with all the cross products.
#' regression_vis(df,
#' 				plot_x_mean = TRUE,
#' 				plot_y_mean = TRUE,
#' 				plot_positive_cross_products = TRUE,
#' 				plot_negative_cross_products = TRUE)
#'
regression_vis <- function(
		df,
		x_var = names(df)[1],
		y_var = names(df)[2],
		plot_x_mean = TRUE,
		plot_y_mean = TRUE,
		# Regression
		plot_regression = FALSE,
		regression_line_color = 'grey30',
		# Deviations
		plot_x_deviations = NULL,
		plot_y_deviations = NULL,
		x_deviation_color = 'darkblue',
		y_deviation_color = 'darkblue',
		deviation_size = 1.5,
		# Cross products
		plot_positive_cross_products = FALSE,
		plot_negative_cross_products = FALSE,
		plot_cross_products = NULL,
		positive_cross_product_fill = 'lightblue',
		negative_cross_product_fill = 'darkred',
		positive_cross_product_color = cross_product_color,
		negative_cross_product_color = cross_product_color,
		cross_product_color = 'grey30',
		cross_product_fill = '#F5C710',
		cross_product_alpha = 0.1,
		# Residuals
		plot_residuals = NULL,
		plot_residuals_squared = NULL,
		residuals_squared_color = 'grey30',
		residuals_squared_fill = 'green',
		residuals_squared_alpha = 0.1,
		residual_color = 'darkred',
		residual_size = 1.5
) {
	x_mean <- mean(df[,x_var], na.rm = TRUE)
	y_mean <- mean(df[,y_var], na.rm = TRUE)
	lm_out <- lm(as.formula(paste0(y_var, ' ~ ', x_var)), data = df)
	df$residual <- resid(lm_out)
	df$predicted <- predict(lm_out)
	df$residual_x <- (df$residual / sd(df[,y_var]) ) * sd(df[,x_var])

	p <- ggplot(df) +
		geom_point(aes(x = .data[[x_var]], y = .data[[y_var]]), size = 3, shape = 1)

	if(plot_x_mean) {
		p <- p + geom_vline(xintercept = mean(df[,x_var], na.rm = TRUE), linetype = 3)
	}
	if(plot_y_mean) {
		p <- p + geom_hline(yintercept = mean(df[,y_var], na.rm = TRUE), linetype = 3)
	}
	if(plot_regression) {
		p <- p + geom_smooth(aes(x = .data[[x_var]], y = .data[[y_var]]),
							 se = FALSE,
							 color = regression_line_color,
							 method = 'lm', formula = y ~ x)
	}
	if(plot_positive_cross_products) {
		df_positive <- which((df[,x_var] > x_mean & df[,y_var] > y_mean) |
			(df[,x_var] < x_mean & df[,y_var] < y_mean))

		p <- p +
			geom_rect(data = df[df_positive,],
					  aes(xmin = .data[[x_var]], ymin = .data[[y_var]],
					  	xmax = x_mean, ymax = y_mean),
					  fill = positive_cross_product_fill,
					  color = positive_cross_product_color,
					  alpha = cross_product_alpha)
	}
	if(plot_negative_cross_products) {
		df_negative <- which((df[,x_var] < x_mean & df[,y_var] > y_mean) |
			(df[,x_var] > x_mean & df[,y_var] < y_mean))
		p <- p +
			geom_rect(data = df[df_negative,],
					  aes(xmin = .data[[x_var]], ymin = .data[[y_var]],
					  	  xmax = x_mean, ymax = y_mean),
					  fill = negative_cross_product_fill,
					  color = negative_cross_product_color,
					  alpha = cross_product_alpha)
	}
	if(!is.null(plot_cross_products)) {
		p <- p +
			geom_rect(data = df[plot_cross_products,],
					  aes(xmin = .data[[x_var]], ymin = .data[[y_var]],
					  	  xmax = x_mean, ymax = y_mean),
					  fill = cross_product_fill,
					  color = cross_product_color,
					  alpha = cross_product_alpha)
	}
	if(!is.null(plot_residuals_squared)) {
		p <- p +
			geom_rect(data = df[plot_residuals_squared,],
					  aes(xmin = .data[[x_var]],
					  	  ymin = .data[[y_var]],
					  	  xmax = .data[[x_var]] - .data[['residual_x']],
					  	  ymax = .data[['predicted']]),
					  fill = residuals_squared_fill,
					  color = residuals_squared_color,
					  alpha = residuals_squared_alpha)
	}
	if(!is.null(plot_residuals)) {
		p <- p +
			geom_segment(data = df[plot_residuals,],
						 aes(x = .data[[x_var]], y = .data[[y_var]],
						 	 xend = .data[[x_var]], yend = .data[['predicted']]),
						 color = residual_color,
						 linewidth = residual_size)
	}
	if(!is.null(plot_x_deviations)) {
		p <- p +
			geom_segment(data = df[plot_x_deviations,],
						 aes(x = .data[[x_var]], y = .data[[y_var]],
						 	 xend = x_mean, yend = .data[[y_var]]),
						 arrow = arrow(length = unit(0.5, "cm")),
						 color = x_deviation_color,
						 linewidth = deviation_size)
	}
	if(!is.null(plot_y_deviations)) {
		p <- p +
			geom_segment(data = df[plot_y_deviations,],
						 aes(x = .data[[x_var]], y = .data[[y_var]],
						  	 xend = .data[[x_var]], yend = y_mean),
						 arrow = arrow(length = unit(0.5, "cm")),
						 color = y_deviation_color,
						 linewidth = deviation_size)
	}


	p <- p + theme_vs()

	return(p)
}

if(FALSE) {
	library(ggplot2)
	df <- mtcars[,c('wt', 'mpg')]
	cross_products <- abs(df[,1] * df[,2])
	cross_products == max(cross_products) # Find the largest cross product

	# Scatter plot with arrows showing the largest cross product
	regression_vis(df,
					plot_x_mean = TRUE,
					plot_y_mean = TRUE,
					plot_positive_cross_products = FALSE,
					plot_negative_cross_products = FALSE,
					plot_regression = TRUE,
					#plot_cross_products = cross_products == max(cross_products),
					# plot_residuals = cross_products == max(cross_products),
					plot_residuals_squared = cross_products == max(cross_products)
	)

	regression_vis(df,
					plot_x_mean = TRUE,
					plot_y_mean = TRUE,
					plot_positive_cross_products = FALSE,
					plot_negative_cross_products = FALSE,
					plot_regression = TRUE,
					#plot_cross_products = cross_products == max(cross_products),
					# plot_residuals = cross_products == max(cross_products),
					plot_residuals_squared = TRUE,
					plot_residuals = TRUE
	)

}
