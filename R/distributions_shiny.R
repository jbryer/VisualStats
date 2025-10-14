#' Run the Shiny server for visualizing distributions
#'
#' This Shiny application provides an interactive interface to the [plot_distributions()] function.
#'
#' @param ... other parameters passed to [shiny::shinyApp()]
#' @return results of shinyApp
#' @export
#' @rdname distributions_shiny
distributions_shiny <- function(...) {
	shiny::shinyApp(ui = distributions_shiny_ui,
					server = distributions_shiny_server,
					...)
}

#' Distributions Shiny UI
#'
#' @return a Shiny UI object.
#' @export
#' @rdname distributions_shiny
distributions_shiny_ui <- function() {
	# data("distributions", package = 'VisualStats')
	distributions <- VisualStats::distributions
	dist_choices <- distributions$function_name
	names(dist_choices) <- paste0(distributions$label, ' (', distributions$function_name, ')')

	navbarPage(
		title = "Distribution Viewer",

		tabPanel(
			"Viewer",
			sidebarLayout(
				sidebarPanel(
					selectInput(
						inputId = 'distribution',
						label = 'Distribution',
						choices = dist_choices,
						selected = 'norm'),
					uiOutput('dist_params')
				),

				mainPanel(
					# textOutput("dist_label"),
					plotOutput("dist_plot", height = '500px')
				)
			)
		),
		tabPanel(
			"Help",
			includeMarkdown(paste0(find.package('VisualStats'), '/shiny/distributions2/help.md'))
		)
	)
}

#' Distributions Shiny Server
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @return a function with Shiny server logic.
#' @export
#' @rdname distributions_shiny
distributions_shiny_server <- function(input, output, session) {
	# data("distributions", package = 'VisualStats')
	distributions <- VisualStats::distributions
	output$fun_params_ui <- renderUI({
		params <- formals(distributions[[input$distribution]]$fun)
		params_default <- formals(distribution_plot)
		fun_params <- names(params)[!names(params) %in% names(params_default)]

		result <- tagList()
		for(i in fun_params) {
			result[[i]] <- numericInput(i, label = i,
										value = eval(params[[i]], params),
										step = 1)
		}

		return(result)
	})

	get_distribution <- reactive({
		req(input$distribution)
		dist <- distributions[distributions$function_name == input$distribution,]
		if(nrow(dist) != 1) {
			stop(paste0('Unexpected number of distributions returned, should be 1, have ', nrow(dist)))
		}
		return(dist)
	})

	output$dist_label <- renderText({
		dist <- get_distribution()
		dist[1,]$label
	})

	output$dist_params <- renderUI({
		dist <- get_distribution()
		args <- eval(parse(text = paste0('list(', dist[1,]$arguments, ')')))
		# xvals <- eval(parse(text = paste0('as.numeric(c(', dist[1,]$xvals, '))')))
		inputs <- list()
		inputs[[length(inputs) + 1]] <- textInput(
			inputId = 'xvals',
			label = 'x-values (comma separated)',
			value = dist[1,]$xvals
		)
		inputs[[length(inputs) + 1]] <- numericInput(
			inputId = 'xmin',
			label = 'x-min',
			value = dist[1,]$xmin
		)
		inputs[[length(inputs) + 1]] <- numericInput(
			inputId = 'xmax',
			label = 'x-max',
			value = dist[1,]$xmax
		)
		for(i in seq_len(length(args))) {
			if(is.numeric(args[[i]])) {
				inputs[[length(inputs) + 1]] <- numericInput(
					inputId = names(args)[i],
					label = names(args)[i],
					value = args[[i]]
				)
			} else {
				inputs[[length(inputs) + 1]] <- textInput(
					inputId = names(args)[i],
					label = names(args)[i],
					value = args[[i]]
				)
			}
		}
		do.call(what = div, args = inputs)
	})

	output$dist_plot <- renderPlot({
		req(input$xmin); req(input$xmax); req(input$xvals)

		dist <- get_distribution()
		args <- eval(parse(text = paste0('list(', dist[1,]$arguments, ')')))
		arg_vals <- list()
		for(i in names(args)) {
			arg_vals[[i]] <- input[[i]]
		}
		xvals <- eval(parse(text = paste0('as.numeric(c(', input$xvals, '))')))

		tryCatch({
			plot_distributions(
				dist[1,]$function_name,
				xvals = xvals,
				xmin = input$xmin,
				xmax = input$xmax,
				args = arg_vals
			)
		}, error = function(e) {
			# Ignoring. Error happens when changing distributions and the parameter UI elements
			# haven't been created yet.
		})
	})
}
