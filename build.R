library(devtools)
library(usethis)
library(testthat)

##### Build and install the package
document()
install(build_vignettes = FALSE)
install(build_vignettes = TRUE)
check()

devtools::clean_vignettes()
devtools::build_vignettes()

# This function will cleanup the DESCRIPTION file to be tidy
usethis::use_tidy_description()

# Add dependency
# usethis::use_package('ShinyDemo', type = 'Imports')
# usethis::use_package('vdiffr', type = 'Suggests')

##### Tests
# Create a test
usethis::use_test('loess-test')


##### Render the README and generate the pkgdown website
rmarkdown::render('README.Rmd', output_format = 'github_document')
pkgdown::build_site()


##### Check exported functions and data
ls('package:VisualStats')
data(package = 'VisualStats')

data("hand_washing")
anova_vis(hand_washing$Bacterial_Counts, hand_washing$Method)
anova_vis(hand_washing$Bacterial_Counts, hand_washing$Method,
		  plot_boxplot = TRUE)


##### Vignettes
vignette(package = 'VisualStats')
vignette('anova', package = 'VisualStats')


##### Shiny Apps
shiny_demo()
shiny_demo('variance')
shiny_demo('anova')
shiny_demo('loess')
