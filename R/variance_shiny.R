#' Visualizing Variance Shiny Application
#'
#' This will start a shiny app explore variance visually.
#'
#' @param ... other parameters passed to [shiny::runApp]
#' @references http://rstudio.com/shiny
#' @export
variance_shiny <- function(...) {
	shiny::runApp(app, ...)
}

#' Server function for variance application
#' @export
variance_shiny_ui <- function() {
	fluidPage(
		titlePanel("Visualize Sum of Squares and Variance"),

		sidebarLayout(
			sidebarPanel(
				checkboxGroupInput('plot_features',
								   'Plot Features:',
								   choices = c(
								   	'Mean' = 'plot_mean',
								   	'Deviances' = 'plot_deviances',
								   	'Population Variance (divide by N)' = 'plot_population_variance',
								   	'Sample Variance (divide by N - 1)' = 'plot_sample_variance',
								   	'Population Standard Deviation' = 'plot_population_sd',
								   	'Sample Standard Deviation' = 'plot_sample_sd'

								   )),
				selectInput('variance_position',
							'Variance Position:',
							choices = c('Top' = 'top',
										'Middle' = 'middle',
										'Bottom' = 'bottom'))
			),

			mainPanel(
				plotOutput("plot", height = '600px')
			)
		)
	)
}

#' UI function for variance application
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object
#' @export
variance_shiny_server <- function(input, output, session) {
	getData <- reactive({
		x <- c(97.88, 107.91, 88.26, 115.21, 87.38)
	})

	output$plot <- renderPlot({
		x <- getData()

		variance_vis(
			x,
			plot_mean = 'plot_mean' %in% input$plot_features,
			plot_deviances = 'plot_deviances' %in% input$plot_features,
			plot_sample_variance = 'plot_sample_variance' %in% input$plot_features,
			plot_population_variance = 'plot_population_variance' %in% input$plot_features,
			plot_sample_sd = 'plot_sample_sd' %in% input$plot_features,
			plot_population_sd = 'plot_population_sd' %in% input$plot_features,
			variance_position = input$variance_position
		)
	})
}

