Website: <https://jbryer.github.io/VisualStats/>

This package contains functions and [Shiny](https://shiny.rstudio.com)
applications designed to visualize statistical tests. The latest version
can be installed from Github using the `remotes` package:

    remotes::install_github('jbryer/VisualStats')

The vignettes currently available:

    library(VisualStats)
    vignette(package = 'VisualStats')[['results']][,c('Item', 'Title'), drop = FALSE]

    ##      Item    Title                                          
    ## [1,] "anova" "Graphical Analysis of Variance (source, html)"
    ## [2,] "loess" "Loess Regression (source, html)"

Shiny apps available. You can run the apps using the
`shiny_demo('APP_NAME')` where `APP_NAME` is from the list below.

    shiny_demo()

    ##       package      app
    ## 1 VisualStats    anova
    ## 2 VisualStats    loess
    ## 3 VisualStats variance

All available functions and datasets:

    ls('package:VisualStats')

    ## [1] "anova_vis"    "hand_washing" "loess_vis"    "renderRmd"    "shiny_demo"   "variance_vis"
