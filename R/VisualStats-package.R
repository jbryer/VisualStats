utils::globalVariables(c('df', 'hand_washing', 'anorexia', 'npk', 'iris', 'penguins',
					   'Vlaue', 'Group', 'square', 'contrast', 'MS', 'boxplot',
					   'lower_hinge', 'upper_hinge', 'median' ,'lower_whisker', 'upper_whisker',
					   'xmin', 'ymin', 'xmax', 'ymax', 'sd', 'Mean.Square',
					   'dbeta', 'dchisq', 'V1', 'V2', 'x', 'y',
					   'x_deviation', 'y_deviation', 'sd', 'cross_product',
					   'x1', 'x2', 'y1', 'y2', 'height', 'predicted', 'residual',
						'Value', 'item', 'faithful', 'weight', 'mtcars', 'xend', 'yend',
						'Iteration', 'value', 'variable', 'd', 'x_pos', 'color', 'boxTidwell_digits',
						'boxTidwell_position_x', 'boxTidwell_position_y'))


#' Color palette used for visualizations (qualitative)
#' @export
vs_palette_qual <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999')

#' Color palette used for visualizations (sequential and blue)
#' @export
vs_palette_seq_blues <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')

#' Color palette used for visualizations (sequential and purples)
#' @export
vs_palette_seq_purples <- c('#feebe2','#fbb4b9','#f768a1','#c51b8a','#7a0177')

#' Color palette used for visualizations (sequential and greens)
#' @export
vs_palette_seq_greens <- c('#ffffcc','#c2e699','#78c679','#31a354','#006837')

#' ggplot2 theme for the VisualStats package
#' @export
#' @return a ggplot2 theme
theme_vs <- function() {
	theme_minimal() +
	theme(panel.grid.major = element_line(color = 'grey90', size = 0.3),
		  panel.grid.minor = element_blank(),
		  legend.position = 'bottom')
}

#' Visualizations for Statistical Analysis
#'
#' @name VisualStats-package
#' @docType package
#' @title Visualizations for Statistical Analysis
#' @author \email{jason@@bryer.org}
#' @keywords statistics visualizations shiny
#' @import magrittr
#' @import ggplot2
#' @import reshape2
#' @import knitr
#' @import markdown
#' @import gganimate
#' @import cowplot
#' @import egg
#' @import shiny
#' @importFrom tibble tibble
#' @importFrom dplyr rename mutate filter summarize summarise n group_by ungroup relocate across all_of
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom psych describe describeBy
#' @importFrom utils data
#' @importFrom grDevices boxplot.stats
#' @importFrom stats median sd var mad IQR lm resid predict anova as.formula anova rnorm optim
#'             binomial complete.cases dist dnorm dt fitted glm loess pf quantile runif setNames
#'             t.test qt qnorm
"_PACKAGE"

#' Bacteria counts after various hand washing techniques.
#'
#' From chapter 28 of De Veaux, Velleman, and Bock (2009):
#'
#' "A student decided to investigate just how effective washing with soap is in
#' eliminating bacteria. To do this she tested four different methods -- washing
#' with water only, washing with regular soap, washing with antibacterial soap (ABS),
#' and spraying hands with antibacterial spray (AS) (containing 65% ethanol as
#' an active ingredient). Her experiement consisted of one experimental factor,
#' the washing *Method*, and four levels."
#'
#' @name hand_washing
#' @docType data
#' @format a data frame with 32 ovservations of 2 variables.
#' \describe{
#'   \item{Bacterial_Counts}{the bacterial count after washing hands}
#'   \item{Method}{the method of handwashing used}
#' }
#' @keywords datasets
#' @references De Veaux, R.D., Velleman, P.F., & Bock, D.E. (2009). *Intro Stats* (4th ed). Pearson.
NA

#' Depression, anxiety and affect
#'
#' The data set used in this set of visualizations has been provided by Professor
#' [James Boswell](https://www.albany.edu/psychology/faculty/james-boswell),
#' and is from a psychological treatment study. Baseline indices of Depression severity, Anxiety
#' Severity, and Positive Affect were acquired on a sample of clients prior to treatment. These
#' simple measurements permit investigation of the relative and interactive importance of Anxiety
#' and Positivity in their relationship to Depression scores. It is not surprising that Anxiety
#' Scores and Depression scores are positively related.
#'
#' The interesting question is whether Positive Affect moderates this relationship. The analysis
#' presented in these visualizations is superficial and is only intended for instructional purposes
#' in seeing interaction and Simple Slopes. Any strong scientific conclusions based on these
#' depictions would be premature.
#'
#' See https://bcdudek.net/shinyapps.html for more information.
#'
#' @name depression
#' @docType data
#' @format a data frame with 200 observations of 3 variables.
#' \describe{
#'     \item{depression}{Depression severity}
#'     \item{anxiety}{Anxiety severity}
#'     \item{affect}{Positive affect}
#' }
#' @keywords datasets
NA


#' Product strength predicted from processing time, temperature, and pressure
#'
#' This data set is designed to explore the interactions effects. The independent variables
#' (processing time, temperature, and pressure) affect the dependent variable (product strength).
#' There is an interaction between `Temperature` and `Pressure`.
#'
#' @name manufacturing
#' @docType data
#' @format a data frame with 29 observations of 4 variables
#' \describe{
#'     \item{Strength}{}
#'     \item{Temperature}{}
#'     \item{Pressure}{}
#'     \item{Time}{}
#' }
#' @references Frost, J. (2020). *Regression Analysis: An Intuitive Guide for Using and Interpreting
#' Linear Models* Statistics By Jim Publishing.
#' https://statisticsbyjim.com/regression/interaction-effects/
NA

#' Various characteristics of States (and District of Columbia) including poverty
#'
#' This dataset was originally used by Gelman and Hill (2007) to demonstrate multiple regression.
#' In particular multicollinearity predicting `poverty` from `female_house` and `white`.
#'
#' @name poverty
#' @docType data
#' @format a data frame with 51 observations of 6 variables
#' \describe{
#'     \item{state}{The state.}
#'     \item{metro_res}{Percent of people in the state living in a metropolitan area.}
#'     \item{white}{Percent of the state population that is white.}
#'     \item{hs_grad}{Percent of the state population that has a high school diploma.}
#'     \item{poverty}{Percent of the state population living in poverty.}
#'     \item{female_house}{Percent of the state where the female is head of household.}
#' }
#' @references Gelman, H. & Hill, J. (2007). *Data Analysis using Regression and Multilevel/Hierarchical Models.*
#' Cambridge University Press.
NA
