library(devtools)

# create_package(path = '.')
# use_gpl3_license()

document()
install()
install(build_vignettes = TRUE, dependencies = FALSE)
check()

devtools::clean_vignettes()
devtools::build_vignettes()

##### Check exported functions and data
ls('package:VisualStats')
data(package = 'VisualStats')


##### Vignettes
vignette(package = 'VisualStats')
vignette('anova', package = 'VisualStats')


anova_vis(hand_washing$Bacterial.Counts, hand_washing$Method)

##### Shiny Apps
shiny_demo()
shiny_demo('variance')
shiny_demo('anova')
