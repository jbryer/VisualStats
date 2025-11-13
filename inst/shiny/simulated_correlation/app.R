library(shiny)
library(ggplot2)


ui <- fluidPage(
    titlePanel("Simulated sampling distribution of Pearson correlation"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4('Population parameters'),
            numericInput(
                inputId = 'pop_n',
                label = 'Population size (N)',
                value = 100000,
                min = 100,
                max = 200000
            ),
            numericInput(
                inputId = 'mean',
                label = 'Mean',
                value = 0
            ),
            numericInput(
                inputId = 'sd',
                label = 'Standard Deviation',
                value = 1,
                min = 0.0000001
            ),
            sliderInput(
                inputId = 'rho',
                label = 'Rho (ρ)',
                value = 0,
                min = -1,
                max = 1,
                step = 0.05
            ),
            actionButton(
                inputId = 'resample_pop',
                label = 'Resample Population',
                icon = icon('refresh')
            ),
            hr(),
            h4('Sample parameters'),
            sliderInput(
                inputId = 'samp_n',
                label = 'Sample size (n)',
                value = 15,
                min = 2,
                max = 1000,
                step = 1
            ),
            numericInput(
                inputId = 'n_samples',
                label = 'Number of samples',
                value = 1000,
                min = 10,
                max = 10000
            ),
            checkboxInput(
                inputId = 'show_mean_se',
                label = 'Show mean and standard error from the simulated sampling distribution',
                value = FALSE
            ),
            conditionalPanel(
                condition = 'input.show_mean_se',
                sliderInput(
                    inputId = 'confidence_interval',
                    label = 'Confidence interval',
                    value = 0.95,
                    min = 0.01,
                    max = 0.99,
                    step = 0.01
                )
            ),
            sliderInput(
                inputId = 'p_threshold',
                label = 'Shade samples where p <',
                value = 0.05,
                min = 0,
                max = 1,
                step = 0.01
            ),
            radioButtons(
                inputId = 'test_distribution',
                label = 'Test Distribution',
                choices = c('normal', 't'),
                selected = 't'
            ),
            actionButton(
                inputId = 'resample_samples',
                label = 'Refresh Samples',
                icon = icon('refresh')
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel(
                    'Sampling Distribution',
                    plotOutput("distribution_plot", height = '600px')
                ),
                tabPanel(
                    'Population',
                    plotOutput('population_plot', height = '600px')
                )
            )
        )
    )
)

server <- function(input, output) {
    get_population <- reactive({
        input$resample_pop
        # Generate population
        pop <- tibble::tibble(
            x1 = stats::rnorm(input$pop_n, mean = input$mean, sd = input$sd),
            x2 = input$rho * x1 + stats::rnorm(input$pop_n, mean = input$mean, sd = sqrt(1 - input$rho^2))
        ) |> as.data.frame()
        return(pop)
    })

    get_simulated_samp_dist <- reactive({
        input$resample_samples
        pop <- get_population()
        # Data frame to save r and p-value
        sample_r <- data.frame(
            r = numeric(input$n_samples),
            p_t = numeric(input$n_samples),
            p_norm = numeric(input$n_samples)
        )
        for(i in seq_len(nrow(sample_r))) {
            samp <- pop[sample(nrow(pop), size = input$samp_n, replace = FALSE),]
            r <- stats::cor.test(
                samp$x1, samp$x2,
                conf.level = 0.95, # May want to make this a parameter.
                # Would also be good to capture normal as well as t
                method = 'pearson'
            )
            sample_r[i,]$r <- r$estimate
            sample_r[i,]$p_t <- r$p.value
            sample_r[i,]$p_norm <- unname(pnorm(-1 * abs(r$statistic)) * 2)
        }
        return(sample_r)
    })

    output$population_plot <- renderPlot({
        pop <- get_population()
        ggplot(pop, aes(x = x1, y = x2)) +
            geom_point(alpha = 0.01) +
            geom_density2d() +
            geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
            theme_minimal() +
            ggtitle('Scatter plot for population correlation (ρ)',
                    subtitle = paste0(
                        'ρ = ', round(cor(pop$x1, pop$x2), digits = 3), '\n',
                        'Population N = ', prettyNum(input$pop_n, big.mark = ',', format = 'f', scientific = FALSE)
                    ))
    })

    output$distribution_plot <- renderPlot({
        pop <- get_population()
        sample_r <- get_simulated_samp_dist()
        suppressWarnings({
            p_dist <- ifelse(input$test_distribution == 't', 'p_t', 'p_norm')
            low_cut <- max(sample_r[sample_r[,p_dist] < input$p_threshold & sample_r$r < 0,]$r)
            max_cut <- min(sample_r[sample_r[,p_dist] < input$p_threshold & sample_r$r > 0,]$r)
        })

        p <- ggplot(sample_r, aes(x = r)) +
            geom_density() +
            theme_minimal() +
            theme(legend.position = 'none') +
            xlim(c(-1, 1)) +
            ggtitle(
                paste0('Simulated sampling distribution of Pearson correlation (r)'),
                subtitle = paste0(
                    'ρ = ', round(cor(pop$x1, pop$x2), digits = 3), '\n',
                    'Population N = ', prettyNum(input$pop_n, big.mark = ',', format = 'f', scientific = FALSE), '\n',
                    'Sample n = ', input$samp_n, '\n',
                    'n samples = ', prettyNum(input$n_samples, big.mark = ',', format = 'f', scientific = FALSE), '\n',
                    ifelse(input$p_threshold > 0,
                        paste0(
                           'Shaded area represents samples where p < ', input$p_threshold, '\n',
                           'Proportion of samples where p < 0.05 = ',
                           100 * (nrow(sample_r[sample_r[,p_dist] < input$p_threshold,]) / nrow(sample_r)), '%'
                        ),
                        ''
                    ))
            )

        if(input$p_threshold > 0) {
            p <- p +
                stat_density(aes(fill = after_stat(x) |> cut(!!c(low_cut, max_cut)))) +
                scale_fill_manual(values = c('white'), na.value = "steelblue")
        }

        if(input$show_mean_se) {
            r_se <- 0
            if(input$test_distribution == 'normal') {
                r_se <- abs(qnorm((1 - input$confidence_interval)/2)) * sd(sample_r$r)
            } else if(input$test_distribution == 't') {
                r_se <- abs(qt((1 - input$confidence_interval)/2, df = input$samp_n - 1)) * sd(sample_r$r)
            } else {
                stop('Unknown test distribution')
            }
            p <- p +
                geom_vline(xintercept = mean(sample_r$r), color = 'blue', linewidth = 2) +
                geom_segment(y = 0, x = mean(sample_r$r) - r_se, xend = mean(sample_r$r) + r_se,
                             color = 'darkgreen', linewidth = 3)
        }

        return(p)
    })
}

shinyApp(ui = ui, server = server)
