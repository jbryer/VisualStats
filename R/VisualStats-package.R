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
#' @import magick
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
