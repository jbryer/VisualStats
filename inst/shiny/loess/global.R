library(shiny)
library(tidyverse)
library(shinyBS)
library(shinyWidgets)

data('faithful', package = 'datasets')

# Max value for span should be 1 when there is only one predictor.
span_range <- c(0.05, 1)
