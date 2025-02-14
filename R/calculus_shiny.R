default_fun = c('Normal' = '1 / (sqrt(2 * pi)) * exp(1)^(-1/2 * (x)^2)',
				'Cubic' = '0.25 * x^3 + 0.25 * x^2 + 0.49 * x + 5')

#' Shiny UI
#'
#' @return a Shiny UI object.
#' @export
#' @rdname calculus_shiny
calculus_shiny_ui <- function() {
	# Shiny UI code here. Basic siderbar panel provided
	fluidPage(
		sidebarLayout(
			sidebarPanel(
				selectInput('calc_type', label = 'Choose...', choices = c('Integrals', 'Derivatives')),
				selectInput('functions', label = 'Functions', choices = names(default_fun)),
				uiOutput('fun'),
				fluidRow(
					column(6, uiOutput('min_input')),
					column(6, uiOutput('max_input'))
				),
				hr(),
				conditionalPanel(condition = 'input.calc_type == "Integrals"',
								 uiOutput('range_input'),
								 sliderInput("boxes",
								 			label = 'Number of boxes',
								 			min = 1, max = 200,
								 			step = 1, value = 3)
				),
				conditionalPanel(condition = 'input.calc_type == "Derivatives"',
								 uiOutput('point_input'),
								 # numericInput('delta_x', label = 'delta_x',
								 # 			 min = -5, max = 5, value = -1, step = 0.1)
								 sliderInput('delta_x', label = 'Delta x',
								 			min = -1, max = 1, value = -1, step = 0.05)
				)
			),

			mainPanel(
				textOutput('error_msg'),
				plotOutput("plot"),
				conditionalPanel(condition = 'input.calc_type == "Integrals"',
								 p('Using the integrate function:', textOutput('area'))
				),
				conditionalPanel(condition = 'input.calc_type == "Derivatives"'
				)
			)
		)
	)
}

#' Shiny server
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @return a function with Shiny server logic.
#' @importFrom stats integrate
#' @export
#' @rdname calculus_shiny
calculus_shiny_server <- function(input, output, session) {
	error_message <- reactiveVal('')

	output$error_msg <- renderText({
		error_message()
	})

	getFunction <- reactive({
		tryCatch({
			eval(parse(text = paste('f <- function(x) { return(' , input$fun , ')}', sep='')))
			error_message('')
		}, error = function(e) {
			print(e)
			error_message('Error in function specification.')
		})
		return(f)
	})

	output$min_input <- renderUI({
		numericInput(inputId = 'view_min',
					 label = 'Plot min',
					 value = -4, step = 1,
					 min = -100, max = 0)
	})

	output$max_input <- renderUI({
		numericInput(inputId = 'view_max',
					 label = 'Plot max',
					 value = 4, step = 1,
					 min = 0, max = 100)
	})

	output$range_input <- renderUI({
		req(input$view_min); req(input$view_max)
		sliderInput("range",
					label = 'Range',
					min = input$view_min, max = input$view_max,
					value = c(0, 2),
					step = 0.1)
	})

	output$point_input <- renderUI({
		req(input$view_min); req(input$view_max)
		sliderInput('point',
					label = 'Point',
					value = input$view_min + (input$view_max - input$view_min) / 4,
					min = input$view_min,
					max = input$view_max,
					step = 0.2)
	})

	output$fun <- renderUI({
		textInput('fun', label = 'Function', value = default_fun[input$functions])
	})

	output$plot <- renderPlot({
		req(input$view_min); req(input$view_max)
		fun <- getFunction()
		if(is.null(fun)) { return(); }

		p <- NULL
		if(input$calc_type == 'Integrals') {
			req(input$range); req(input$boxes)

			p <- integral_plot(fun,
							   xmin = input$range[1],
							   xmax = input$range[2],
							   n = input$boxes) +
				xlim(c(input$view_min, input$view_max))
		} else if(input$calc_type == 'Derivatives') {
			req(input$point); req(input$delta_x)

			# dx <- Deriv(f, "x") # TODO: Why doesn't this work!

			p <- derivative_plot(fun = fun,
								 x_value = input$point,
								 delta_x = input$delta_x,
								 view_xmin = input$view_min,
								 view_xmax = input$view_max)
		}
		return(p)
	})

	output$area <- renderPrint({
		req(input$range)
		f <- getFunction()
		if(is.null(f)) { return(); }
		stats::integrate(f, input$range[1], input$range[2])
	})
}

#' Run the Shiny server
#' @param ... other parameters passed to [shiny::shinyApp]
#' @export
#' @rdname calculus_shiny
calculus_shiny <- function(...) {
	shiny::shinyApp(ui = calculus_shiny_ui, server = calculus_shiny_server, ...)
}
