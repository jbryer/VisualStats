library(shiny)
library(tidyverse)
library(VisualStats)

ui <- navbarPage(
	id = 'navbarpage',
	title = "Graphical One-Way Analysis of Variance (ANOVA)",
	#theme = 'readable',
	#theme = shinythemes::shinytheme("simplex"),
	# shinythemes::themeSelector(),

	tabPanel(
		'Plot',
		# chooseSliderSkin("Shiny", color = "seagreen"),
		tags$head(tags$style(
			HTML(
				'.irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
								  visibility: hidden !important; }'
			)
		)),

		sidebarLayout(
			sidebarPanel(
				selectInput(
					'dataset',
					'Select a dataset: ',
					choices = c('handwashing', 'anorexia', 'npk', 'simulate'),
					selected = 'handwashing'
				),
				hr(),
				checkboxGroupInput(
					'plot_features',
					'Plot Features:',
					choices = c(
						'Unit Line' = 'unit_line',
						'Grand Mean' = 'grand_mean',
						'Grand (overall) Standard Deviation' = 'sd_line',
						'Boxplot' = 'boxplot',
						'Within Group Standard Deviations' = 'group_sd',
						'Within Group Variances' = 'group_variances',
						'Mean Square Within (Error)' = 'ms_within',
						'Pooled Within Group Standard Deviation' = 'pooled_sd',
						'Pooled Between Group Variances' = 'between_group_variances',
						'Mean Square Between (Treatment)' = 'ms_between'
					)
				),
				hr(),
				conditionalPanel(
					'input.dataset == "simulate"',
					sliderInput(
						'k',
						'Number of groups:',
						min = 2,
						max = 10,
						value = 3,
						step = 1
					),
					numericInput(
						'n',
						'n per group:',
						min = 2,
						max = 1000,
						value = 10,
						step = 1
					),
					conditionalPanel('input.dataset == "simulate"',
									 uiOutput('mean_ui')),
					numericInput('sd',
								 'Standard Deviation:',
								 value = 3),
					actionButton('resample', 'Resample')
				),
				conditionalPanel('input.dataset != "simulate"',
								 uiOutput('mean_adjust_ui'))
			),

			mainPanel(plotOutput("plot", height = '600px'))
		),
	),


	tabPanel('About',
			 # withMathJax(includeHTML(paste0(find.package('VisualStats'), '/doc/loess.html')))
			 htmlOutput('about'))
)

server <- function(input, output, session) {
	getData <- reactive({
		req(input$dataset)
		input$resample

		df <- NULL
		if (input$dataset == 'simulate') {
			req(input$n)
			req(input$k)
			req(input$mean1)
			group_means <- numeric(input$k)
			for (i in 1:input$k) {
				group_means[i] <- input[[paste0('mean', i)]]
			}
			df <- data.frame(Group = rep(LETTERS[1:input$k], each = input$n),
							 Value = as.numeric(sapply(
							 	group_means,
							 	FUN = function(x) {
							 		rnorm(input$n, mean = x, sd = input$sd)
							 	}
							 )))
		} else if (input$dataset == 'handwashing') {
			data("hand_washing")
			df <- data.frame(
				Group = hand_washing$Method,
				Value = as.integer(hand_washing$Bacterial_Counts)
			)
		} else if (input$dataset == 'anorexia') {
			data(anorexia, package = 'MASS')
			df <- data.frame(Group = anorexia$Treat,
							 Value = anorexia$Postwt - anorexia$Prewt)
		} else if (input$dataset == 'npk') {
			data(npk, package = 'datasets')
			df <- data.frame(Group = npk$block,
							 Value = npk$yield)
		}

		return(df)
	})

	output$mean_ui <- renderUI({
		inputs <- list()
		for (i in 1:input$k) {
			inputs[[paste0('mean_', i)]] <- numericInput(
				paste0('mean', i),
				paste0('Group ', LETTERS[i], ' mean:'),
				value = i,
				step = 1
			)
		}
		return(inputs)
	})

	getGroupName <- function(name) {
		gsub(' ', '_', name)
	}

	output$mean_adjust_ui <- renderUI({
		input$dataset
		inputs <- list()
		isolate(df <- getData())
		grand_sd <- sd(df$Value)
		for (i in unique(df$Group)) {
			value <- mean(df[df$Group == i, ]$Value)
			isolate(if (!is.null(input[[paste0('mean_adjust_', getGroupName(i))]])) {
				value <- input[[paste0('mean_adjust_', getGroupName(i))]]
			})

			# TODO: Sliders for adding/subtracting values to all values.
			# https://stackoverflow.com/questions/35251788/hide-values-of-sliderinput-in-shiny

			# The rounding here will cause the values to change and the initial statistics to be wrong
			# inputs[[paste0('mean_adjust_', getGroupName(i))]] <- sliderInput(paste0('mean_adjust_', getGroupName(i)),
			# 												   paste0('Change mean for ', i, ' to:'),
			# 												   value = value,
			# 												   min = round(-1 * grand_sd * 4),
			# 												   max = round(grand_sd * 4) )
			# inputs[[paste0('mean_adjust_', getGroupName(i))]] <- sliderInput(paste0('mean_adjust_', getGroupName(i)),
			# 																 paste0('Change mean for ', i, ' to:'),
			# 																 value = value,
			# 																 hide_min_max = TRUE,
			# 																 min = (-1 * grand_sd * 4),
			# 																 max = (grand_sd * 4) )

			inputs[[paste0('mean_adjust_', getGroupName(i))]] <-
				numericInput(
					paste0('mean_adjust_', getGroupName(i)),
					paste0('Change mean for ', i, ' to:'),
					value = value
				)
		}

		inputs[['adjust_reset']] <-
			actionButton('adjust_reset', 'Reset Adjustments')

		return(inputs)
	})

	observeEvent(input$adjust_reset, {
		isolate(df <- getData())
		for (i in unique(df$Group)) {
			value <- mean(df[df$Group == i, ]$Value)
			updateSliderInput(
				session = session,
				inputId = paste0('mean_adjust_', getGroupName(i)),
				value = value
			)
		}
	})

	output$plot <- renderPlot({
		df <- getData()

		if (input$dataset != 'simulate') {
			for (i in unique(df$Group)) {
				req(input[[paste0('mean_adjust_', getGroupName(i))]])

				df[df$Group == i, ]$Value <- df[df$Group == i, ]$Value +
					input[[paste0('mean_adjust_', getGroupName(i))]] - mean(df[df$Group == i, ]$Value)
			}
		}

		anova_vis(
			df$Value,
			df$Group,
			plot_boxplot = 'boxplot' %in% input$plot_features,
			plot_group_variances = 'group_variances' %in% input$plot_features,
			plot_group_sd = 'group_sd' %in% input$plot_features,
			plot_ms_within = 'ms_within' %in% input$plot_features,
			plot_ms_between = 'ms_between' %in% input$plot_features,
			plot_unit_line = 'unit_line' %in% input$plot_features,
			plot_grand_mean = 'grand_mean' %in% input$plot_features,
			plot_sd_line = 'sd_line' %in% input$plot_features,
			plot_pooled_sd = 'pooled_sd' %in% input$plot_features,
			plot_between_group_variances = 'between_group_variances' %in% input$plot_features
		)
	})

	output$about <- VisualStats::renderRmd(paste0(find.package('VisualStats'), '/doc/anova.Rmd'),
										   input,
										   envir = environment())

}

shinyApp(ui = ui, server = server)
