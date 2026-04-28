library(shiny)
library(VisualStats)

ui <- fluidPage(
    titlePanel("Overlapping Confidence Intervals for Independent Sample Tests"),

    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(
            	inputId = 'plot_options',
            	label = 'Plot Options',
            	choices = c(
            		'plot_distribuition',
            		'plot_means',
            		'plot_se',
            		'plot_zero_difference',
            		'plot_ci',
            		'plot_points'
            	),
            	selected = c(
            		'plot_points'
            	)
            )
        ),

        mainPanel(
           plotOutput("independent_sample_vis")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
	get_data <- reactive({
		n <- 20
		mean1 <- 10
		sd1 <- 2
		mean2 <- 14
		sd2 <- 2
		df <- data.frame(
			group = c(rep('Group A', n / 2),
					  rep('Group B', n / 2)),
			value = c(rnorm(n = (n / 2), mean = mean1, sd = sd1),
					  rnorm(n = (n / 2), mean = mean2, sd = sd2))
		)
		return(df)
	})

    output$independent_sample_vis <- renderPlot({
		df <- get_data()
    	independent_sample_vis(
    		df,
    		plot_se = 'plot_se' %in% input$plot_options,
    		plot_distribuition = 'plot_distribuition' %in% input$plot_options,
    		plot_zero_difference = 'plot_zero_difference' %in% input$plot_options,
    		plot_ci = 'plot_ci' %in% input$plot_options,
    		plot_means = 'plot_means' %in% input$plot_options,
    		plot_points = 'plot_points' %in% input$plot_options
    	)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
