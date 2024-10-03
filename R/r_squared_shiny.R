#' Run the Shiny server for visualizing R-Squared
#'
#' @param ... other parameters passed to [shiny::shinyApp()]
#' @return results of shinyApp
#' @export
r_squared_shiny <- function(...) {
	shiny::shinyApp(ui = r_squared_shiny_ui,
					server = r_squared_shiny_server,
					...)
}

#' R-Squared Shiny UI
#'
#' @return a Shiny UI object.
#' @export
r_squared_shiny_ui <- function() {
	fluidPage(
		sidebarLayout(
			sidebarPanel(
				selectInput(
					'dataset',
					'Select a dataset: ',
					choices = c('simulate', 'mtcars'),
					selected = 'simulate'
				),
				conditionalPanel(
					'input.dataset == "simulate"',
					sliderInput(
						'r_squared',
						'R-Squared:',
						min = 0,
						max = 1,
						value = .5,
						step = .05
					),
					numericInput(
						'n',
						'Sample size:',
						min = 10,
						max = 1000,
						value = 200,
						step = 1
					),
					actionButton('resample', 'Resample')
				),
				hr(),
				checkboxGroupInput(
					'plot_features',
					'Plot Features:',
					choices = c(
						'Points' = 'plot_points',
						'Unit Line (y = x)' = 'plot_unit_line',
						'Means' = 'plot_means',
						'Residuals' = 'plot_residuals',
						'Squared Residuals' = 'plot_residuals_squared',
						'Total Variance' = 'plot_total_variance',
						'Error Variance' = 'plot_error_variance',
						'Regression Variance' = 'plot_regression_variance',
						'All Variances' = 'plot_all_variances'
					),
					selected = c('plot_points', 'plot_means', 'plot_unit_line')
				)
			),

			mainPanel(
				tabsetPanel(
					tabPanel('Plot',
							plotOutput("plot", height = '600px') ),
					tabPanel('Regression Output',
							 verbatimTextOutput('regression_out') ),
					tabPanel('Variance Plot',
							 plotOutput('variance_plot', height = '600px') )
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
r_squared_shiny_server <- function(input, output, session) {
	getData <- reactive({
		req(input$dataset)
		input$resample

		df <- NULL
		if(input$dataset == 'simulate') {
			req(input$n)
			req(input$r_squared)
			df <- VisualStats::simulate(n = input$n, r_squared = input$r_squared)
		} else if(input$dataset == 'mtcars') {
			data('mtcars', envir = environment())
			df <- mtcars
		}
		return(df)
	})

	getFormula <- reactive({
		if(input$dataset == 'simulate') {
			return(y ~ x1 + x2)
		} else if(input$dataset == 'mtcars') {
			return(mpg ~ wt + cyl + hp)
		}
	})

	output$regression_out <- renderPrint({
		lm(getFormula(), data = getData()) |> summary()
	})

	output$plot <- renderPlot({
		r_squared_vis(
			getData(),
			getFormula(),
			plot_total_variance = 'plot_total_variance' %in% input$plot_features,
			plot_regression_variance = 'plot_regression_variance' %in% input$plot_features,
			plot_error_variance = 'plot_error_variance' %in% input$plot_features,
			plot_all_variances = 'plot_all_variances' %in% input$plot_features,
			plot_unit_line = 'plot_unit_line' %in% input$plot_features,
			plot_points = 'plot_points' %in% input$plot_features,
			plot_means = 'plot_means' %in% input$plot_features,
			plot_residuals = 'plot_residuals' %in% input$plot_features,
			plot_residuals_squared = 'plot_residuals_squared' %in% input$plot_features
		)
	})

	output$variance_plot <- renderPlot({
		df <- getData()
		formu <- getFormula()
		VisualStats::variance_vis(df[,all.vars(formu)[1],drop=TRUE])
	})
}
