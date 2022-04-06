if(FALSE) { # for debugging
    input <- list()
    input$dataset <- 'cubic'
    input$center <- -1
    input$degree <- 1
    input$span <- .3
    input$show_loess <- TRUE
}

shinyServer(function(input, output) {
    # Returns the data.frame. The first column will be plotted on the x-axis,
    # second on the y-axis
    getData <- reactive({
        req(input$dataset)
        df <- NULL
        if(input$dataset == 'cubic') {
            df <- tibble(
                x = seq(-1, 1, by = 0.01),
                y = x^3 + rnorm(length(x), mean = 0, sd = 0.05) - x
            )
        } else if(input$dataset == 'quadratic') {
            df <- tibble(
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

    output$about <- ShinyDemo::renderRmd(
            paste0(find.package('VisualStats'), '/doc/loess.Rmd'),
            input,
            envir=environment())

    output$center_input <- renderUI({
        df <- getData()
        fluidPage(sliderInput("center",
                    "Center:",
                    min = min(df[,1]),
                    max = max(df[,1]),
                    value = (min(df[,1])),
                    # value = min(df[,1]) + (diff(range(df[,1])) / 4),
                    step = diff(range(df[,1])) / 20,
                    round = TRUE,
                    animate = animationOptions(interval = 800, loop = TRUE))
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
                  center = input$center)
    })

})
