#' Run the Shiny server for visualizing R-Squared
#'
#' @param ... other parameters passed to [shiny::shinyApp()]
#' @return results of shinyApp
#' @export
#' @rdname dependent_sample_shiny
dependent_sample_shiny <- function(...) {
	shiny::shinyApp(ui = dependent_sample_shiny_ui,
					server = dependent_sample_shiny_server,
					...)
}

#' R-Squared Shiny UI
#'
#' @return a Shiny UI object.
#' @export
#' @rdname dependent_sample_shiny
dependent_sample_shiny_ui <- function() {
	fluidPage(
		sidebarLayout(
			sidebarPanel(
				selectInput(
					'dataset',
					'Select a dataset: ',
					choices = c('simulate', 'anorexia', 'blood_lead'),
					selected = 'anorexia'
				),
				selectInput(
					'test',
					label = 'Test distribution:',
					choices = c('norm', 't'),
					selected = 'norm'
				),
				sliderInput(
					'conf_level',
					label = 'Confidence Level:',
					min = 0.01, max = 0.99, step = 0.01, value = 0.95
				),
				# numericInput(
				# 	'height_multiplier',
				# 	'Height Multiplier:',
				# 	min = 10, max = 200, value = 40
				# ),
				conditionalPanel(
					'input.dataset == "simulate"',
					numericInput(
						'mean_x',
						'Mean x:',
						value = 100
					),
					numericInput(
						'sd_x',
						'Std Dev x:',
						value = 15
					),
					numericInput(
						'mean_y',
						'Mean y:',
						value = 110
					),
					numericInput(
						'sd_y',
						'Std Dev y:',
						value = 15
					),
					numericInput(
						'n',
						'Sample size:',
						min = 10,
						max = 1000,
						value = 30,
						step = 1
					),
					actionButton('resample', 'Resample')
				),
				hr(),
				checkboxGroupInput(
					'plot_features',
					'Plot Features:',
					choices = c(
						'Unit Line (y = x)' = 'plot_unit_line',
						'Projections' = 'plot_projections',
						'Differences' = 'plot_differences',
						'Mean difference' = 'plot_mean',
						'Sampling Distribution' = 'plot_samp_dist',
						'Confidence Interval' = 'plot_ci',
						'Confidence Interval Lines' = 'plot_ci_lines'
					),
					selected = c('plot_unit_line')
				)
			),

			mainPanel(
				tabsetPanel(
					tabPanel('Plot',
							plotOutput("plot", height = '600px') )
					)
				)
			)
		)
}

#' R-Squared Shiny Server
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @return a function with Shiny server logic.
#' @export
#' @rdname dependent_sample_shiny
dependent_sample_shiny_server <- function(input, output, session) {
	getData <- reactive({
		req(input$dataset)
		input$resample

		df <- NULL
		if(input$dataset == 'simulate') {
			req(input$n)
			df <- data.frame(
				x = rnorm(input$n, mean = input$mean_x, sd = input$sd_x),
				y = rnorm(input$n, mean = input$mean_y, sd = input$sd_y)
			)
		} else if(input$dataset == 'anorexia') {
			data('anorexia.sub', package = 'granova', envir = baseenv())
			df <- anorexia.sub
		} else if(input$dataset == 'blood_lead') {
			data('blood_lead', package = 'granova', env = baseenv())
			df <- blood_lead
		}
		return(df)
	})

	output$plot <- renderPlot({
		dependent_sample_vis(
			getData(),
			conf_level = input$conf_level,
			test = input$test,
			# height_multiplier = input$height_multiplier,
			plot_mean = 'plot_mean' %in% input$plot_features,
			plot_unit_line = 'plot_unit_line' %in% input$plot_features,
			plot_projections = 'plot_projections' %in% input$plot_features,
			plot_differences = 'plot_differences' %in% input$plot_features,
			plot_ci = 'plot_ci' %in% input$plot_features,
			plot_ci_lines = 'plot_ci_lines' %in% input$plot_features,
			plot_samp_dist = 'plot_samp_dist' %in% input$plot_features
		)
	})

}
