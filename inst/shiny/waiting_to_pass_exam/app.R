library(shiny)
library(ggplot2)

set.seed(2112)
options(scipen = 999)

theme_set(theme_minimal())

#' Simulate how long until a specified number of responses are correct
#' @param size test size.
#' @param prob probability of randomly getting correct answer
#' @param stop_score the score on the test we wish to achieve. Value of 1
#'        indicates a perfect score.
simulate_test <- function(size, p, stop_score = 1) {
	n <- 0
	repeat{
		n <- n + 1
		test <- sample(c(TRUE, FALSE),
					   size = size,
					   prob = c(p, 1 - p),
					   replace = TRUE)
		if(mean(test) >= stop_score) {
			break
		}
	}
	return(n)
}

##### UI ###########################################################################################
ui <- fluidPage(
    titlePanel("How many times do I need to take a test to randomly get all questions correct?"),

    sidebarLayout(
        sidebarPanel(
            sliderInput(
            	inputId = 'test_size',
            	label = 'Number of questions',
            	value = 5,
            	min = 2,
            	max = 10,
            	step = 1
            ),
            sliderInput(
            	inputId = 'n_options',
            	label = 'Number of question options',
            	value = 4,
            	min = 2,
            	max = 5
            ),
            sliderInput(
            	inputId = 'pass_rate',
            	label = 'Passing threshold',
            	value = 1,
            	min = 0,
            	max = 1
            ),
            sliderInput(
            	inputId = 'prob_of_success',
            	label = 'Probability of success test attempt',
            	value = 0.5,
            	min = 0.01,
            	max = 0.99
            ),
            sliderInput(
            	inputId = 'n_simulations',
            	label = 'Number of simulations',
            	value = 100,
            	min = 20,
            	max = 1000,
            	step = 20
            ),
            actionButton(
            	inputId = 'run_simulation',
            	label = 'Run simulation'
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
        	tabsetPanel(
        		tabPanel(
        			title = 'Binomial',
        			plotOutput('binomial_plot')
        		),
        		tabPanel(
        			title = 'Geometric',
        			plotOutput('geomotric_plot')
        		),
        		tabPanel(
        			title = 'Simulation',
        			plotOutput('simulation_plot')
        		)
        	)
        )
    )
)

##### Server #######################################################################################
server <- function(input, output) {
	# output$binoial_text <- renderText({
	# 	paste0(
	# 		'The probability of getting ', (100 * input$pass_rate), '%',
	# 		' of questions correct where there is a ,' (100 * (1 / input$n_options)),
	# 		'% change of getting each question raondomly currect is ',
	# 		dbinom(x = , size = input$test_size, prob = 1 / input$n_options)
	# 	)
	# })

	output$binomial_plot <- renderPlot({
		dist <- dbinom(x = 0:input$test_size, size = input$test_size, prob = 1 / input$n_options)
		df <- data.frame(
			x = 0:input$test_size,
			prob = dist,
			label = paste0(round(100 * dist, digits = 2), '%'))
		ggplot(df, aes(x = x, y = prob, label = label)) +
			geom_bar(stat = 'identity', fill = 'grey50') +
			geom_text(data = df[df$prob > 0.0001,], vjust = -0.5) +
			ggtitle('Binomial distribution')
	})

	output$geomotric_plot <- renderPlot({
		prob <- dbinom(x = input$test_size,
					   size = input$test_size,
					   prob = 1 / input$n_options)
		max_x <- qgeom(0.99, prob = prob)
		geom_dist <- data.frame(
			x = 0:max_x,
			y = dgeom(0:max_x, prob = prob))
		cut_point <- qgeom(input$prob_of_success, prob = prob)
		ggplot(geom_dist, aes(x = x, y = y)) +
			geom_polygon(data = rbind(data.frame(x = 0, y = 0),
									  geom_dist[geom_dist$x < cut_point,],
									  data.frame(x = cut_point, y = 0)),
						 fill = 'grey70') +
			geom_path(stat = 'identity', color = 'blue') +
			ggtitle('Geometric distribution')
	})

	simulation_result <- reactiveVal(value = NULL)

	observeEvent(input$n_simulations | input$n_options | input$test_size, {
		simulation_result(NULL)
	})

	observeEvent(input$run_simulation, {
		isolate({
			simulations <- integer(input$n_simulations)
			for(i in 1:length(simulations)) {
				simulations[i] <- simulate_test(size = input$test_size, p = 1 / input$n_options)
			}
			simulation_result(simulations)
		})
	})

	output$simulation_plot <- renderPlot({
		simulations <- simulation_result()
		if(!is.null(simulations)) {
			ggplot(data.frame(x = simulations), aes(x = x)) +
				geom_histogram(aes(y = after_stat(density)), bins = 50, fill = 'grey70') +
				geom_density(color = 'blue') +
				ggtitle('Distribution of simulation results')
		}
	})
}

##### Run App ######################################################################################
shinyApp(ui = ui, server = server)
