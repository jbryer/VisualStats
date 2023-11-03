shinyUI(navbarPage(
    id = 'navbarpage',
    title = "Loess Smoothing",
    #theme = 'readable',
    #theme = shinythemes::shinytheme("simplex"),
    # shinythemes::themeSelector(),

    tabPanel(
        'Plot',
        chooseSliderSkin("Shiny", color="seagreen"),
        sidebarLayout(
            sidebarPanel(
                selectInput('dataset',
                            'Dataset:',
                            choices = c('Quadratic plus random noise' = 'quadratic',
                                        'Cubic plus random noise' = 'cubic',
                                        'Old Faithful' = 'faithful')),
                uiOutput('center_input'),
                selectInput('degree',
                            'Degree:',
                            choices = c('Linear' = 1,
                                        'Quadratic' = 2)),
                numericInput('span',
                             'Span:',
                             min = span_range[1],
                             max = span_range[2],
                             step = .05,
                             value = .3),
                checkboxInput('draw_loess', label = 'Draw Loess fit (best used with animation on the slider)', value = FALSE),
                checkboxInput('show_loess', label = 'Show Full Loess fit', value = FALSE),
                helpText(" "),
                helpText("Click your mouse on the plot to see an explanation of the graph.  (Remove with another click)"),
            ),

            mainPanel(
                plotOutput("loess_plot"),
                bsPopover(id="loess_plot",
                          title="Description",
                          placement="left",
                          trigger="click",
                          content = paste0("This app is a visual/conceptual demonstration of the elements of a  Loess Plot.",
                                           "The orange point plots the predicted value from an X axis value midway along a local regression ",
                                           "shown by the local (green) regression fit.  The center of this locally weighted regression and its X axis span ",
                                           "can be controlled by the CENTER slider and SPAN numeric entry box.",
                                           "<br><br>",
                                           "Best use of the app would use the animation control for the CENTER slider ",
                                           "and the checkbox for actively drawing the Loess fit.",
                                           "<br><br>",
                                           "The full loess fit can also be displayed with the second checkbox.",
                                           "<br><br>",
                                           "The local regression can be specifed as a linear or quadratic fit with the DEGREE dropdown. ",
                                           "<br><br>",
                                           "Also see the ABOUT tab."))
            )
        )
    )

    # tabPanel(
    #     'About',
    #     # withMathJax(includeHTML(paste0(find.package('VisualStats'), '/doc/loess.html')))
    #     htmlOutput('about')
    # )
))
