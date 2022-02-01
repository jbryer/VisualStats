library(devtools)

##### Build and install the package
document()
install(build_vignettes = TRUE, dependencies = FALSE)
check()

devtools::clean_vignettes()
devtools::build_vignettes()


##### Render the README and generate the packagedown website
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
