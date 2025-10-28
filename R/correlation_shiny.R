#' Shiny application for visualization of correlations and cross products.
#'
#' @param ... other parameters passed to [shiny::shinyApp()].
#' @export
#' @rdname correlation_shiny
correlation_shiny <- function(...) {
	shiny::shinyApp(ui = correlation_shiny_ui,
					server = correlation_shiny_server,
					...)
}


#' Shiny UI for correlation
#'
#' @return a Shiny UI object.
#' @export
#' @rdname correlation_shiny
correlation_shiny_ui <- function() {
	fluidPage(
		sidebarLayout(
			sidebarPanel(
				fluidRow(
					column(
						width = 6,
						numericInput(
							inputId = 'mean_x',
							label = 'IV (x) mean',
							value = 20, min = 100, max = 100, step = 1
						)
					),
					column(
						width = 6,
						numericInput(
							inputId = 'sd_x',
							label = 'IV (x) Std. Dev.',
							value = 2, min = 0, max = 100, step = 1
						)
					)
				),
				fluidRow(
					column(
						width = 6,
						numericInput(
							inputId = 'mean_y',
							label = 'DV (y) mean',
							value = 30, min = 100, max = 100, step = 1
						)
					),
					column(
						width = 6,
						numericInput(
							inputId = 'sd_y',
							label = 'DV (y) Std. Dev.',
							value = 3, min = 0, max = 100, step = 1
						)
					)
				),
				numericInput(
					inputId = 'n',
					label = 'Sample Size',
					value = 30
				),
				sliderInput(
					inputId = 'rho',
					label = 'Population Correlation',
					value = 0.8,
					min = -1, max = 1, step = 0.01
				),
				checkboxGroupInput(
					inputId = 'plot_features',
					label = 'Plot Features:',
					choices = c(
						'Use standard scores' = 'standard_scores',
						'Plot mean of x' = 'plot_x_mean',
						'Plot mean of y' = 'plot_y_mean',
						'Plot rectangles for positive cross products' = 'plot_positive_cross_products',
						'Plot rectangles for negative cross products' = 'plot_negative_cross_products',
						'Show line of best fit (regression line)' = 'plot_regression',
						'Plot all residuals' = 'plot_all_residuals',
						'Plot all squared residuals' = 'plot_all_residuals_squared'
					),
					selected = c('plot_x_mean', 'plot_y_mean')
				),
				radioButtons(
					inputId = 'selection_type',
					label = 'Point selection:',
					choices = c('Cross products' = 'cross_product',
								'Squared residuals' = 'squared_residual')
				),
				fluidRow(
					column(
						width = 5,
						actionButton(
							inputId = 'resample',
							label = 'Resample'
						)
					),
					column(
						width = 7,
						actionButton(
							inputId = 'clear_selection',
							label = 'Clear Selection'
						)
					)
				)
			),

			mainPanel(
				tabsetPanel(
					tabPanel(
						'Scatter Plot',
						plotOutput("plot", height = '600px', click = "scatter_plot_click")
					),
					tabPanel(
						'Cross Products',
						plotOutput('cross_product_histogram', height = '600px')
					),
					tabPanel(
						'Residuals',
						plotOutput('residual_histogram', height = '600px')
					)
				)
			)
		)
	)
}

#' Shiny server for maximum likelihood estimation
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @return a function with Shiny server logic.
#' @export
#' @importFrom dplyr mutate
#' @importFrom mvtnorm rmvnorm
#' @rdname correlation_shiny
correlation_shiny_server <- function(input, output, session) {
	get_data <- reactive({
		input$resample
		selected_rows(NULL)
		mvtnorm::rmvnorm(
			n = input$n,
			mean = c(input$mean_x, input$mean_y),
			sigma = matrix(c(input$sd_x^2, input$rho * (input$sd_x * input$sd_y),
							 input$rho * (input$sd_x * input$sd_y), input$sd_y^2), 2, 2)) |>
			as.data.frame() |>
			dplyr::rename(x = V1, y = V2) |>
			dplyr::mutate(x_deviation = x - mean(x),
						  y_deviation = y - mean(y),
						  cross_product = x_deviation * y_deviation)
	})

	output$plot <- renderPlot({
		thedata <- get_data()

		if('standard_scores' %in% input$plot_features) {
			thedata[,1] <- (thedata[,1] - mean(thedata[,1])) / sd(thedata[,1])
			thedata[,2] <- (thedata[,2] - mean(thedata[,2])) / sd(thedata[,2])
		}

		residuals <- NULL
		if('plot_all_residuals' %in% input$plot_features) {
			residuals <- rep(TRUE, nrow(thedata))
		} else if(input$selection_type == 'squared_residual') {
			residuals <- selected_rows()
		}
		residuals_squared <- NULL
		if('plot_all_residuals_squared' %in% input$plot_features) {
			residuals_squared <- rep(TRUE, nrow(thedata))
		} else if(input$selection_type == 'squared_residual') {
			residuals_squared <- selected_rows()
		}
		cross_products <- NULL
		if(input$selection_type == 'cross_product') {
			cross_products <- selected_rows()
		}

		p <- regression_vis(
			df = thedata,
			plot_x_mean = 'plot_x_mean' %in% input$plot_features,
			plot_y_mean = 'plot_y_mean' %in% input$plot_features,
			plot_positive_cross_products = 'plot_positive_cross_products' %in% input$plot_features,
			plot_negative_cross_products = 'plot_negative_cross_products' %in% input$plot_features,
			plot_regression = 'plot_regression' %in% input$plot_features,
			plot_cross_products = cross_products,
			plot_residuals = residuals,
			plot_residuals_squared = residuals_squared
		)

		if('standard_scores' %in% input$plot_features) {
			p <- p + coord_equal()
		}

		row <- selected_rows()
		if(!is.null(row)) {
			if(input$selection_type == 'cross_product') {
				center_x <- thedata[row,]$x - (thedata[row,]$x_deviation / 2)
				center_y <- thedata[row,]$y - (thedata[row,]$y_deviation / 2)
				p <- p +
					geom_text(x = center_x,
							  y = center_y,
							  label = round(thedata[row,]$cross_product, digits = 4))
			} else if(input$selection_type == 'squared_residual') {
				# print(thedata[row,])
			}
		}

		return(p)
	})

	output$cross_product_histogram <- renderPlot({
		thedata <- get_data()
		ggplot(thedata, aes(x = cross_product, fill = cross_product > 0)) +
			geom_histogram(bins = 15, alpha = 0.75) +
			scale_fill_manual('Cross product > 0', values = c('lightblue', 'darkred')) +
			xlab('Cross Product') +
			theme_vs()
	})

	output$residual_histogram <- renderPlot({
		thedata <- get_data()
		lm_out <- lm(y ~ x, data = thedata)
		thedata$residuals <- resid(lm_out)
		ggplot(thedata, aes(x = residuals, y = after_stat(density))) +
			geom_histogram(bins = 10, alpha = 0.75) +
			geom_density(linewidth = 2) +
			xlab('Residuals') +
			theme_vs()
	})

	selected_rows <- reactiveVal(NULL)

	observeEvent(input$clear_selection, {
		selected_rows(NULL)
	})

	observe({
		thedata <- get_data()
		if('standard_scores' %in% input$plot_features) {
			thedata[,1] <- (thedata[,1] - mean(thedata[,1])) / sd(thedata[,1])
			thedata[,2] <- (thedata[,2] - mean(thedata[,2])) / sd(thedata[,2])
		}
		row <- nearPoints(thedata,
						  input$scatter_plot_click,
						  maxpoints = 1,
						  addDist = FALSE,
						  xvar = names(thedata)[1],
						  yvar = names(thedata)[2])
		if(nrow(row) > 0) {
			selected_rows(row.names(thedata) == row.names(row))
		}
	})
}
