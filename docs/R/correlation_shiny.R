#' Shiny application for visualization of correlations and cross products.
#'
#' @param ... other parameters passed to [shiny::shinyApp()].
#' @export
correlation_shiny <- function(...) {
	shiny::shinyApp(ui = correlation_shiny_ui,
					server = correlation_shiny_server,
					...)
}


#' Shiny UI for correlation
#'
#' @return a Shiny UI object.
#' @export
correlation_shiny_ui <- function() {
	fluidPage(
		sidebarLayout(
			sidebarPanel(
				fluidRow(
					column(
						width = 6,
						numericInput(
							inputId = 'x_mean',
							label = 'IV (x) mean',
							value = 20, min = 100, max = 100, step = 1
						)
					),
					column(
						width = 6,
						numericInput(
							inputId = 'x_sd',
							label = 'IV (x) Std. Dev.',
							value = 2, min = 0, max = 100, step = 1
						)
					)
				),
				fluidRow(
					column(
						width = 6,
						numericInput(
							inputId = 'y_mean',
							label = 'DV (y) mean',
							value = 30, min = 100, max = 100, step = 1
						)
					),
					column(
						width = 6,
						numericInput(
							inputId = 'y_sd',
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
						'Plot mean of x' = 'plot_x_mean',
						'Plot mean of y' = 'plot_y_mean',
						'Plot rectangles for positive cross products' = 'plot_positive_cross_products',
						'Plot rectangles for negative cross products' = 'plot_negative_cross_products',
						'Show line of best fit (regression line)' = 'plot_regression'
					),
					selected = c('plot_x_mean', 'plot_y_mean')
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
				plotOutput("plot", height = '600px', click = "scatter_plot_click")
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
correlation_shiny_server <- function(input, output, session) {
	get_data <- reactive({
		input$resample
		selected_rows(NULL)
		mvtnorm::rmvnorm(n = input$n,
						 mean = c(input$x_mean, input$y_mean),
						 sigma = matrix(c(input$x_sd^2, input$rho * (input$x_sd * input$y_sd),
							   			  input$rho * (input$x_sd * input$y_sd), input$y_sd^2), 2, 2)) |>
			as.data.frame() |>
			dplyr::mutate(cross_products = abs(V1 * V2))
	})

	output$plot <- renderPlot({
		thedata <- get_data()
		correlation_vis(
			df = thedata,
			plot_x_mean = 'plot_x_mean' %in% input$plot_features,
			plot_y_mean = 'plot_y_mean' %in% input$plot_features,
			plot_positive_cross_products = 'plot_positive_cross_products' %in% input$plot_features,
			plot_negative_cross_products = 'plot_negative_cross_products' %in% input$plot_features,
			plot_regression = 'plot_regression' %in% input$plot_features,
			plot_cross_products = selected_rows()
		)
	})

	selected_rows <- reactiveVal(NULL)

	observeEvent(input$clear_selection, {
		selected_rows(NULL)
	})

	observe({
		thedata <- get_data()
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
