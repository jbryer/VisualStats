#' Shiny UI
#'
#'
shiny_ui <-  function() { navbarPage(
	title = "Maximum Likelihood Estimation",
	tabPanel('Main',
			 sidebarLayout(
			 	sidebarPanel(width = 3,
			 				 uiOutput('outcome_ui'),
			 				 uiOutput('predictor_ui'),
			 				 uiOutput('highlight_ui'),
			 				 hr(),
			 				 uiOutput('iteration_ui'),
			 				 checkboxInput('showOLSRegression',
			 				 			  'Show Regression',
			 				 			  value = TRUE)
			 	),

			 	mainPanel(width = 9,
			 			  tabsetPanel(
			 			  	tabPanel('Plots',
			 			  			 fluidRow(
			 			  			 	column(6, plotOutput("scatter_plot", click = "scatter_plot_click",)),
			 			  			 	# column(6, plotOutput('likelihood_plot'))
			 			  			 	column(6, plotOutput("parameter_plot"))
			 			  			 )
			 			  			 # plotOutput("parameter_plot")
			 			  	),
			 			  	tabPanel('Summary',
			 			  			 h4('MLE Results'),
			 			  			 verbatimTextOutput('mle_summary'),
			 			  			 h4('lm or glm Results'),
			 			  			 verbatimTextOutput('summary'),
			 			  			 h4('Point Info'),
			 			  			 verbatimTextOutput("click_info")
			 			  	),
			 			  	tabPanel('Likelihood Plots',
			 			  			 plotOutput('likelihood_plots', height = '600px')),
			 			  	tabPanel('Data',
			 			  			 DT::dataTableOutput('datatable'))
			 			  )
			 	)
			 )
	),

	# tabPanel('Description', includeHTML('mle.html')
	# ),

	tabPanel('About',
			 includeMarkdown(paste0(find.package('visualMLE'), '/shiny/about.md')))
)}
