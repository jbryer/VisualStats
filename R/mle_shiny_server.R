#' Shiny server.
#'
#'
shiny_server <- function(input, output, session) {
	# Can use a different data set if desired
	if(!exists('thedata')) {
		message('No data specified, using mtcars...')
		data(mtcars, envir = environment())
		thedata <- mtcars
		default_y <- 'mpg' # Default variable selected for the dependent variable
		default_x <- 'wt'  # Default variable selected for the independent variable
	}

	# Keep only numeric and logical columns
	numeric.cols <- sapply(thedata, FUN = function(x) { return(is.numeric(x) | is.logical(x)) })
	thedata <- thedata[,numeric.cols]

	default_x <- mget('default_x', ifnotfound = names(thedata)[2])
	default_y <- mget('default_y', ifnotfound = names(thedata)[1])

	# NOTE: This app will use row.names to identify and highlight specific points.
	#       Make sure the row.names are interpretable
	row_highlights <- c('None', row.names(thedata))

	observe({
		row <- nearPoints(thedata, input$scatter_plot_click, maxpoints = 1, addDist = FALSE)
		if(nrow(row) > 0) {
			updateSelectInput(session, 'highlight', selected = rownames(row)[1])
		}
	})

	getData <- reactive({ # Continuous outcome
		req(input$outcome)
		req(input$predictor)
		mle <- NULL
		if(!visualMLE::isBinary(thedata[,input$outcome])) {
			mle <- visualMLE::optim_save(
				runif(3), # Random initial values
				visualMLE::loglikelihood_normal, # likelihood function
				lower = c(-Inf, -Inf, 1.e-5), # The lower bounds for the values, note sigma (error), cannot be negative
				upper = c(Inf, Inf, sd(thedata[,input$predictor]) ),
				method = "L-BFGS-B", # https://en.wikipedia.org/wiki/Nelder–Mead_method
				control = list(fnscale = -1), # Indicates that the maximum is desired rather than the minimum
				predictor = thedata[,input$predictor],
				outcome = thedata[,input$outcome]
			)
		} else { # Logistic Regression
			mle <- visualMLE::optim_save(
				c(0, 1), # Initial values
				visualMLE::loglikelihood_binomial,
				method = "L-BFGS-B",
				control = list(fnscale = -1),
				predictor = thedata[,input$predictor],
				outcome = thedata[,input$outcome]
			)
		}
		return(mle)
	})

	output$outcome_ui <- renderUI({
		selectInput('outcome',
					'Outcome (dependent) variable:',
					choices = names(thedata),
					selected = default_y)
	})

	output$predictor_ui <- renderUI({
		selectInput('predictor',
					'Predictor (independent) variable:',
					choices = names(thedata),
					selected = default_x)
	})

	output$highlight_ui <- renderUI({
		selectInput('highlight',
					'Highlight data point:',
					choices = row_highlights,
					selected = '')
	})

	output$datatable <- DT::renderDataTable({ return(thedata) })

	output$iteration_ui <- renderUI({
		optim_run <- getData()
		sliderInput("iteration",
					"Iteration:",
					min = 1,
					max = length(optim_run$iterations),
					value = 1,
					animate = animationOptions(
						interval = 2000, # TODO: need to find a good pause
						loop = FALSE
					))
	})

	output$scatter_plot <- renderPlot({
		req(input$iteration)
		p <- NULL
		optim_run <- getData()
		if(!visualMLE::isBinary(thedata[,input$outcome])) {
			intercept <- optim_run$iterations[[input$iteration]][1]
			slope <- optim_run$iterations[[input$iteration]][2]
			sigma <- optim_run$iterations[[input$iteration]][3]

			p <- ggplot(thedata, aes_string(x = input$predictor,
											y = input$outcome)) +
				geom_point()
			if(input$showOLSRegression) {
				p <- p + geom_smooth(method = 'lm',
									 formula = y ~ x,
									 color = 'grey30',
									 se = FALSE, size = 2, alpha = 0.5)
			}
			p <- p + geom_abline(intercept = intercept,
								 slope = slope,
								 color = 'blue')
			if(input$highlight != 'None') {
				row <- thedata[input$highlight,,drop = FALSE]

				# p <- plot_likelihood(x = row[, input$predictor],
				# 					 y = row[, input$outcome],
				# 					 intercept = intercept,
				# 					 slope = slope,
				# 					 sigma = sigma)
				p <- p + geom_point(data = row, color = 'red')

				heightFactor <- 1 # Increase the multiplier to make the distribution higher
				k <- 4 # How many standard deviations to plot
				x <- seq(-k * sigma, k * sigma, length.out = 50)
				y <- dnorm(x, 0, sigma) / dnorm(0, 0, sigma) * heightFactor
				x0 <- row[input$highlight, input$predictor]
				y0 <- row[input$highlight, input$outcome]
				yhat <- slope * x0 + intercept
				path <- data.frame(x = y + x0, y = x + yhat)
				# segment <- data.frame(x = x0, y = yhat - k*sigma, xend = x0, yend = yhat + k*sigma)
				segment2 <- data.frame(x = x0,
									   y = y0,
									   xend = dnorm(yhat - y0, 0, sigma) / dnorm(0, 0, sigma) * heightFactor + x0,
									   yend = y0)
				segment3 <- data.frame(x = x0,
									   y = yhat,
									   xend = dnorm(0, 0, sigma) / dnorm(0, 0, sigma) * heightFactor + x0,
									   yend = yhat)

				p <- p +
					# geom_segment(data = segment, aes(x = x, y = y, xend = xend, yend = yend)) +
					geom_segment(data = segment3, aes(x = x, y = y, xend = xend, yend = yend), alpha = 0.3) +
					geom_segment(data = segment2, aes(x = x, y = y, xend = xend, yend = yend), color = 'red') +
					geom_point(data = segment2, aes(x = x, y = y), color = '#e31a1c', size = 2) +
					geom_point(data = segment2, aes(x = xend, y = y), color = '#e31a1c', size = 2) +
					geom_vline(xintercept = x0) +
					# geom_hline(yintercept = y0) + # Verify the distribution is centered on y-hat
					geom_path(data = path, aes(x,y), color = "blue")
			}
		} else {
			beta0 <- optim_run$iterations[[input$iteration]][1]
			beta1 <- optim_run$iterations[[input$iteration]][2]

			p <- ggplot(thedata, aes_string(x = input$predictor,
											y = input$outcome)) +
				geom_point(aes(color = logistic(thedata[,input$predictor], beta0, beta1) > 0.5))

			if(input$showOLSRegression) {
				p <- p + geom_smooth(method = 'glm', formula = y ~ x, se = FALSE,
									 method.args = list(family = binomial(link = 'logit')))
			}
			if(input$highlight != 'None') {
				row <- thedata[input$highlight,]
				segment <- data.frame(x = row[,input$predictor],
									  y = row[,input$outcome],
									  xend = row[,input$predictor],
									  yend = logistic(row[,input$predictor], beta0, beta1))

				p <- p +
					geom_point(data = row, color = 'red', size = 2) +
					geom_segment(data = segment, aes(x = x, xend = xend, y = y, yend = yend),
								 color = 'red')
			}
			p <- p + stat_function(fun = logistic, n = 101,
								   args = list(beta0 = beta0, beta1 = beta1) ) +
				scale_color_hue('Predicted Pass > 0.5') +
				theme(legend.position = c(0.85, 0.15))
		}
		return(p)
	})

	output$parameter_plot <- renderPlot({
		optim_run <- getData()
		df <- optim_run$iterations_df
		if(!visualMLE::isBinary(thedata[,input$outcome])) {
			names(df) <- c('Intercept', 'Slope', 'Sigma', 'LogLikelihood', 'Iteration')
		} else {
			names(df) <- c('Intercept', 'Slope', 'LogLikelihood', 'Iteration')
		}
		df <- reshape2::melt(df, id.var = 'Iteration')
		p <- ggplot(df, aes(x = Iteration, y = value, color = variable)) +
			geom_vline(xintercept = input$iteration)
		p <- p + geom_path()
		p <- p + facet_wrap(~ variable, scales = "free_y", ncol = 1) +
			xlab('Iteration') + ylab('Parameter Estimate') +
			theme(legend.position = 'none')
		return(p)
	})

	output$likelihood_plot <- renderPlot({
		optim_run <- getData()
		p <- NULL
		if(input$highlight != 'None') {
			row <- thedata[input$highlight,]
			df <- optim_run$iterations_df
			if(!visualMLE::isBinary(thedata[,input$outcome])) {
				tmp <- df %>% dplyr::filter(Iteration == input$iteration)
				a <- tmp[1,1]
				b <- tmp[1,2]
				sigma <- tmp[1,3]
				predictor <- row[1,input$predictor]
				predicted.out <- a + b * predictor
				outcome <- row[1,input$outcome]
				d <- dnorm(outcome, predicted.out, sigma)
				p <- ggplot() +
					stat_function(fun = dnorm,
								  n = 101,
								  args = list(mean = predicted.out, sd = sigma)) +
					annotate(geom = 'segment', x = outcome, y = 0, xend = outcome, yend = d, color = 'red') +
					annotate(geom = 'point', x = outcome, y = d, color = 'red', size = 2) +
					xlim(c(min(thedata[,input$predictor], predicted.out - 4 * sigma, outcome),
						   max(thedata[,input$predictor], predicted.out + 4 * sigma, outcome))) +
					xlab('Outcome') + ylab('Density') +
					coord_flip()
			} else {

			}
		}
		return(p)
	})

	output$summary <- renderPrint({
		if(!visualMLE::isBinary(thedata[,input$outcome])) {
			lm.out <- lm(as.formula(paste(input$outcome, ' ~ ', input$predictor)),
						 data = thedata)
			summary(lm.out)
		} else {
			glm.out <- glm(as.formula(paste(input$outcome, ' ~ ', input$predictor)),
						   data = thedata,
						   family = binomial(link = 'logit'))
			summary(glm.out)
		}
	})

	output$mle_summary <- renderPrint({
		optim_run <- getData()
		cat('Final parameters from MLE:', optim_run$par,
			'\nParameters from iteration', input$iteration, ':',
			optim_run$iterations[[input$iteration]], '\n')
	})

	output$click_info <- renderPrint({
		row <- 'Nothing selected.'
		if(input$highlight != 'None') {
			row <- thedata[input$highlight,]
		}
		return(row)
	})

	output$likelihood_plots <- renderPlot({
		df <- getData()$iterations_df
		tmp <- df %>% dplyr::filter(Iteration == input$iteration)
		plots <- list()
		nplots <- nrow(thedata)
		if(!visualMLE::isBinary(thedata[,input$outcome])) {
			for(i in 1:min(nplots, nrow(thedata))) {
				a <- tmp[1,1]
				b <- tmp[1,2]
				sigma <- tmp[1,3]
				predictor <- thedata[i,input$predictor]
				predicted.out <- a + b * predictor
				outcome <- thedata[i,input$outcome]
				d <- dnorm(outcome, predicted.out, sigma)
				plots[[i]] <- ggplot() +
					stat_function(fun = dnorm,
								  n = 101,
								  args = list(mean = predicted.out, sd = sigma)) +
					annotate(geom = 'segment', x = outcome, y = 0, xend = outcome, yend = d, color = 'red') +
					annotate(geom = 'point', x = outcome, y = d, color = 'red', size = 2) +
					xlim(c(min(thedata[,input$outcome], predicted.out - 3 * sigma),
						   max(thedata[,input$outcome], predicted.out + 3 * sigma))) +
					# ylim(c(0, .2)) +
					ggtitle(row.names(thedata)[i]) +
					xlab('Outcome') + ylab('Density')
			}
		} else {
			for(i in 1:min(nplots, nrow(thedata))) {
				beta0 <- tmp[1,1]
				beta1 <- tmp[1,2]
				row <- thedata[i,]
				segment <- data.frame(x = row[,input$predictor],
									  y = row[,input$outcome],
									  xend = row[,input$predictor],
									  yend = logistic(row[,input$predictor], beta0, beta1))

				plots[[i]] <- ggplot(thedata, aes_string(x = thedata[,input$predictor],
														 y = thedata[,input$outcome])) +
					geom_point(data = segment, aes(x = x, y = y), color = 'red', size = 2) +
					geom_segment(data = segment, aes(x = x, xend = xend, y = y, yend = yend),
								 color = 'red') +
					stat_function(fun = logistic, n = 101,
								  args = list(beta0 = beta0, beta1 = beta1) ) +
					ggtitle(row.names(thedata)[i]) +
					xlab('Predictor') + ylab('Outcome')
			}
		}
		title <- ggdraw() +
			draw_label(paste0('Likelihood Distribution Plots for Iteration ', input$iteration), fontface='bold')
		p <- plot_grid(plotlist = plots)
		plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
	})
}
