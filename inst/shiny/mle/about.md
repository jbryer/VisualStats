# Visual Introduction to Maximum Likelihood Estimation

<a href='https://github.com/jbryer/visualMLE'><img src='https://github.com/jbryer/visualMLE/raw/master/visualMLE.png' align="right" height="200" /></a>


**Author:** Jason Bryer, Ph.D. jason.bryer@cuny.edu
**Website:** https://github.com/jbryer/visualMLE

To install the package, run:

```
remotes::install_github('jbryer/visualMLE')
```

Draft document explaining this visual approach to maximum likelihood estimation is available [on Github](https://htmlpreview.github.io/?https://github.com/jbryer/visualMLE/blob/master/vignettes/mle.html) or as a vignette.

```
vignette('mle', package = 'visualMLE')
```

To run the accompanying shiny app, execute the following command:

```
visualMLE::shiny_mle()
```

If you wish to use your own dataset, specify the `df` parameter:

```
visualMLE::shiny_mle(df = faithful)
```

----------

*Acknowledgments:* Thanks to [Bruce Dudek](https://www.albany.edu/psychology/statistics/shinypsych.htm) for comments and suggestions and to [Jorge Cimentada's](https://cimentadaj.github.io) [blog post](https://cimentadaj.github.io/blog/2020-11-26-maximum-likelihood-distilled/maximum-likelihood-distilled/) that inspired me to build this shiny application.
