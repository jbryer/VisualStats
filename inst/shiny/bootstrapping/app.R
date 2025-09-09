library(shiny)
library(VisualStats)

colors <- c(mean = '#66c2a5', median = '#8da0cb', sample = 'grey50')

##### UI ###########################################################################################
ui <- fluidPage(
	titlePanel("Central Limit Theorem and Bootstrapping"),

	sidebarLayout(
		sidebarPanel(
			selectInput(
				inputId = 'population',
				label = 'Population',
				choices = c('Binomial (right skewed)' = 'binom',
							'Uniform' = 'unif',
							'Beta (left skewed)' = 'beta',
							'Normal' = 'norm'),
				selected = 'beta'
			),
			selectInput(
				inputId = 'statistic',
				label = 'Statistic',
				choices = c('Mean' = 'mean',
							'Median' = 'median'),
				selected = 'mean'
			),
			# TODO: Add options for the different distributions
			sliderInput(
				inputId = 'sample_size',
				label = 'Sample Size',
				value = 30,
				min = 10,
				max = 1000,
				step = 10
			),
			sliderInput(
				inputId = 'n_samples',
				label = 'Sampling distribution size',
				value = 100,
				min = 20,
				max = 1000,
				step = 20
			),
			sliderInput(
				inputId = 'n_boot',
				label = 'Number of bootstrap samples',
				value = 100,
				min = 20,
				max = 1000,
				step = 20
			),
			sliderInput(
				inputId = 'bw_adjust',
				label = 'Bandwith adjust',
				min = 0.1,
				max = 3,
				value = 1,
				step = 0.1
			),
			actionButton(
				inputId = 'resample_pop',
				label = 'Resample Population'
			),
			actionButton(
				inputId = 'resample_samp_dist',
				label = 'Resample Sampling Distribution'
			),
			actionButton(
				inputId = 'resample_boot_dist',
				label = 'Resample Bootstrap Distribution'
			)
		),

		# Show a plot of the generated distribution
		mainPanel(
			tabsetPanel(
				tabPanel(
					title = 'Population',
					plotOutput('population_plot')
				),
				tabPanel(
					title = 'Sampling Distribution',
					plotOutput('sampling_dist_plot')
				),
				tabPanel(
					title = 'Bootstrap',
					plotOutput('bootstrap_dist_plot')
				)
			)
		)
	)
)

##### Server #######################################################################################
server <- function(input, output) {
	N <- 100000
	n_digits <- 4

	get_population <- reactive({
		input$resample_pop
		pop <- NULL
		if(input$population == 'binom') {
			pop <- rnbinom(N, 10, .5)
		} else if(input$population == 'unif') {
			pop <- runif(N, 0, 1)
		} else if(input$population == 'beta') {
			pop <- rbeta(N, 5, 2)
		} else if(input$population == 'norm') {
			pop <- rnorm(N)
		} else {
			stop('Unknown population type.')
		}
		return(pop)
	})

	get_sampling_dist <- reactive({
		input$resample_samp_dist
		pop <- get_population()
		samp_dist <- numeric(input$n_samples)
		for(i in 1:n_samples) {
			samp <- sample(pop, size = input$sample_size)
			samp_dist[i] <- do.call(input$statistic, args = list(samp))
		}
		return(samp_dist)
	})

	get_bootstrap_dist <- reactive({
		input$resample_boot_dist
		pop <- get_population()
		samp <- sample(pop, size = input$sample_size)
		boot_dist <- numeric(input$n_boot)
		for(i in 1:length(boot_dist)) {
			boot_samp <- sample(samp, size = length(samp), replace = TRUE)
			boot_dist[i] <- do.call(input$statistic, args = list(boot_samp))
		}
		return(list(sample = samp, boot_dist = boot_dist))
	})

	output$population_plot <- renderPlot({
		pop <- get_population()
		ggplot(data.frame(x = pop), aes(x = x)) +
			geom_vline(xintercept = do.call(input$statistic, args = list(pop)),
					   color = colors[input$statistic], linewidth = 1.25) +
			geom_dist(pop,
					  adjust = input$bw_adjust,
					  center_fun = NULL) +
			ggtitle('Poulation Distribution',
					subtitle = paste0(input$statistic, ' = ', round(
						do.call(input$statistic, args = list(pop)), n_digits)))
	})

	output$sampling_dist_plot <- renderPlot({
		samp_dist <- get_sampling_dist()
		ggplot() +
			geom_dist(samp_dist,
					  color = colors[input$statistic],
					  adjust = input$bw_adjust) +
			annotate(geom = 'point',
					 x = do.call(input$statistic, args = list(pop)),
					 y = 0,
					 color = colors[input$statistic],
					 fill = colors[input$statistic],
					 size = 4,
					 pch = 22) +
			ggtitle('Sampling distribution',
					subtitle = paste0(input$statistic, ' = ', round(
						do.call(input$statistic, args = list(samp_dist)), n_digits)))
	})

	output$bootstrap_dist_plot <- renderPlot({
		boot_dist <- get_bootstrap_dist()
		ggplot() +
			geom_dist(boot_dist,
					  # color = colors[input$statistic],
					  adjust = input$bw_adjust) +
			annotate(geom = 'point',
					 x =  do.call(input$statistic, args = list(pop)),
					 y = 0,
					 color = colors[input$statistic],
					 fill = colors[input$statistic],
					 size = 4,
					 pch = 22) +
			ggtitle('Bootstrap distribution',
					subtitle = paste0(input$statistic, ' = ', round(
						do.call(input$statistic, args = list(boot_dist$boot_dist)), n_digits)))
	})
}

##### Run App ######################################################################################
shinyApp(ui = ui, server = server)
