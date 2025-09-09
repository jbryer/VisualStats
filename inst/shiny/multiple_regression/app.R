library(shiny)
library(VisualStats)
library(plotly)
library(ggplot2)

data("depression", package = "VisualStats")
data("manufacturing", package = "VisualStats")
data("poverty", package = "VisualStats")

data_sets <- list(
	'depression' = depression[,c('depression', 'anxiety', 'affect')],
	'manufacturing' = manufacturing[,c('Strength', 'Temperature', 'Pressure')],
	'poverty' = poverty[,c('poverty', 'female_house', 'white')]
)

ui <- shiny::fluidPage(
    # Application title
    shiny::titlePanel("Multiple Regression"),

    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
    	shiny::sidebarPanel(
    		shiny::selectInput(
    			inputId = 'dataset',
    			label = 'Dataset:',
    			choices = names(data_sets),
    			selected = names(data_sets)[1]
    		),
    		shiny::selectInput(
    			inputId = 'regression',
    			label = 'Plot regression',
    			choices = c('None', 'Regression', 'Interaction'),
    			selected = 'None'
    		),
    		shiny::conditionalPanel(
    			condition = "input.regression != 'None'",
    			shiny::checkboxInput(
    				inputId = 'residuals',
    				label = 'Plot residuals',
    				value = FALSE
    			),
    			shiny::uiOutput('simple_slope_input')
    		),
        ),

        # Show a plot of the generated distribution
        shiny::mainPanel(
        	shiny::tabsetPanel(
        		shiny::tabPanel(
        			title = 'Plot',
        			plotly::plotlyOutput('plot_output', height = '600px')
        		),
        		shiny::tabPanel(
        			title = 'Regression Output',
        			shiny::verbatimTextOutput('regression_results')
        		)
        	)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
	get_data <- shiny::reactive({
		req(input$dataset)
		return(data_sets[[input$dataset]])
	})

	output$simple_slope_input <- shiny::renderUI({
		data <- get_data()
		choices <- c('None', names(data)[2:3])
		shiny::selectInput(
			inputId = 'simple_slopes',
			label = 'Add simple slopes',
			choices = choices,
			selected = 'None'
		)
	})

	output$plot_output <- plotly::renderPlotly({
		req(input$simple_slopes)
		data <- get_data()
		plot_slopes <- NULL
		if(input$simple_slopes == names(data)[2]) {
			plot_slopes <- 'x1'
		} else if(input$simple_slopes == names(data)[3]) {
			plot_slopes <- 'x2'
		}
		plot_regression <- input$regression %in% c('Regression', 'Interaction')
		multiple_regression_vis(
			y = data[,1],
			x1 = data[,2],
			x2 = data[,3],
			y_lab = names(data)[1],
			x1_lab = names(data)[2],
			x2_lab = names(data)[3],
			plot_regression = plot_regression,
			interaction = input$regression == 'Interaction',
			plot_residuals = ifelse(plot_regression, input$residuals, FALSE),
			plot_slopes = plot_slopes
		)
	})

	output$regression_results <- shiny::renderPrint({
		data <- get_data()
		sign <- ifelse(input$regression == 'Interaction', ' * ', ' + ')
		formu <- as.formula(paste0(names(data)[1], ' ~ ', names(data)[2], sign, names(data)[3]))
		lm(formu, data) |> summary()
	})
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
