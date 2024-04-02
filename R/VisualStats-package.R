#' Color palette used for visualizations (qualitative)
#'
#' @export
vs_palette_qual <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999')

#' Color palette used for visualizations (sequential)
vs_palette_seq_blues <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
vs_palette_seq_purples <- c('#feebe2','#fbb4b9','#f768a1','#c51b8a','#7a0177')
vs_palette_seq_greens <- c('#ffffcc','#c2e699','#78c679','#31a354','#006837')

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
#' @import dplyr
#' @import gganimate
#' @import magick
#' @import cowplot
#' @import egg
#' @importFrom shiny runApp shinyApp
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom psych describe describeBy
#' @importFrom DT dataTableOutput renderDataTable
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
