#' Run the Loess Shiny server
#'
#' @param ... other parameters passed to [shiny::shinyApp]
#' @export
loess_shiny <- function(...) {
	shiny::shinyApp(ui = loess_shiny_ui, server = loess_shiny_server, ...)
}

#' Shiny Server
#' @return a function with Shiny server logic.
#' @export
#' @importFrom shinyWidgets chooseSliderSkin
loess_shiny_ui <- function() {
	span_range <- c(0.05, 10)
	fluidPage(
		sidebarLayout(
			sidebarPanel(
				selectInput('dataset',
							'Dataset:',
							choices = c('Quadratic plus random noise' = 'quadratic',
										'Cubic plus random noise' = 'cubic',
										'Old Faithful' = 'faithful')),
				uiOutput('center_input'),
				selectInput('degree',
							'Degree:',
							choices = c('Linear' = 1,
										'Quadratic' = 2)),
				numericInput('span',
							 'Span:',
							 min = span_range[1],
							 max = span_range[2],
							 step = .05,
							 value = 0.75),
				checkboxInput('draw_loess', label = 'Draw Loess fit (best used with animation on the slider)', value = FALSE),
				checkboxInput('show_loess', label = 'Show Full Loess fit', value = FALSE)
			),

			mainPanel(
				plotOutput("loess_plot", height = '500px')
			)
		)
	)
}

#' Shiny UI
#' @return a Shiny UI object.
#' @export
loess_shiny_server <- function(input, output, session) {
	span_range <- c(0.05, 10)
	data('faithful', package = 'datasets')

	# Returns the data.frame. The first column will be plotted on the x-axis,
	# second on the y-axis
	getData <- reactive({
		req(input$dataset)
		df <- NULL
		if(input$dataset == 'cubic') {
			df <- tibble::tibble(
				x = seq(-1, 1, by = 0.01),
				y = x^3 + rnorm(length(x), mean = 0, sd = 0.05) - x
			)
		} else if(input$dataset == 'quadratic') {
			df <- tibble::tibble(
				x = seq(-1, 1, by = 0.01),
				y = -x^2 + rnorm(length(x), mean = 0, sd = 0.1) - x
			)
		} else if(input$dataset == 'faithful') {
			df <- data.frame(
				x = faithful$waiting,
				y = faithful$eruptions
			)
			names(df) <- c('Inter-eruption interval',
						   'Eruption length')
		} else {
			stop('No dataset specified.')
		}
		return(df)
	})

	output$center_input <- renderUI({
		df <- getData()
		sliderInput("center",
					"Center:",
					min = min(df[,1]),
					max = max(df[,1]),
					value = (min(df[,1])),
					# value = min(df[,1]) + (diff(range(df[,1])) / 4),
					step = diff(range(df[,1])) / 20,
					round = TRUE,
					animate = animationOptions(interval = 800, loop = TRUE)
		)
	})

	validateInput <- reactive({
		validate(
			need(input$span >= span_range[1] & input$span <= span_range[2],
				 paste0(' Please select a span between ', span_range[1], ' and ', span_range[2], '.'))
		)
	})

	output$loess_plot <- renderPlot({
		validateInput()
		req(input$degree)
		req(input$span)
		req(input$center)

		df <- getData()
		df$x <- df[,1,drop=TRUE]
		df$y <- df[,2,drop=TRUE]

		loess_vis(formula = y ~ x,
				  data = df,
				  degree = input$degree,
				  draw_loess = input$draw_loess,
				  show_loess = input$show_loess,
				  center = input$center,
				  span = input$span)
	})
}
