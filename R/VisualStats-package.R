#' Visualizations for Statistical Tests
#'
#' @name VisualStats-package
#' @docType package
#' @title Visualizations for Statistical Tests
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
