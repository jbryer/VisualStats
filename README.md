VisualStats: R package for visualizing statistical tests
================

This package contains functions and [Shiny](https://shiny.rstudio.com)
applications designed to visualize statistical tests. The latest version
can be installed from Github using the `remotes` package:

``` r
remotes::install_github('jbryer/VisualStats')
```

The vignettes currently available:

``` r
library(VisualStats)
vignette(package = 'VisualStats')[['results']][,c('Item', 'Title'), drop = FALSE]
```

    ##       Item    Title                                          
    ## Topic "anova" "Graphical Analysis of Variance (source, html)"

Shiny apps available. You can run the apps using the
`shiny_demo('APP_NAME')` where `APP_NAME` is from the list below.

``` r
shiny_demo()
```

    ##       package      app
    ## 1 VisualStats    anova
    ## 2 VisualStats variance

All available functions and datasets:

``` r
ls('package:VisualStats')
```

    ## [1] "anova_vis"    "hand_washing" "shiny_demo"   "variance_vis"
