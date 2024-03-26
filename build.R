library(devtools)
library(usethis)
library(testthat)

##### Build and install the package
usethis::use_tidy_description() # Cleanup the DESCRIPTION file to be tidy
devtools::document()
devtools::install()
devtools::check()

devtools::clean_vignettes()
devtools::build_vignettes()


# Add dependency
# usethis::use_package('shinyWidgets', type = 'Imports')
# usethis::use_package('vdiffr', type = 'Suggests')

# quarto add quarto-ext/shinylive

##### Readme
rmarkdown::render('README.Rmd')

##### Book
quarto::quarto_render('book/', output_format = 'html')
# quarto::quarto_render('book/index.qmd', output_format = 'pdf')

# Preview the book (necessary for the Shiny apps to work)
httpuv::runStaticServer(dir = 'docs/', port = 2112)


##### Build shinylive versions of the apps
# NOTE: This doesn't currently work very well. Some app work, but not others.
library(VisualStats)

shiny_apps <- list.dirs('inst/shiny/')

app_files <- c('app', 'global', 'ui', 'sever')
build_dir <- 'build_shiny/'
out_dir <- 'docs/'
app_index <- ''

for(i in shiny_apps) {
	files <- list.files(i)
	if(any(app_files %in% tools::file_path_sans_ext(files))) {
		app_name <- basename(i)
		message(paste0('Building ', app_name, '...'))
		app_index <- paste0(app_index, '\n<li><a href="', app_name, '/">',
							app_name, '</a></li>')
		# Create the build directory and copy the app over
		dir.create(build_dir, showWarnings = FALSE, recursive = TRUE)
		file.copy(from = i, to = build_dir, recursive = TRUE)
		# Package dependencies
		desc <- utils::packageDescription('VisualStats')
		pkgs <- gsub('\\n', '', desc$Imports) |> strsplit(split = ',') |>
			unlist() |>
			stringr::str_trim()
		pkgs <- paste0('library(', pkgs, ')', collapse = '\n')
		# loop through the shiny app files and replace VisualStats:: with function
		# call and add function source to the script. Also add dependencies
		files <- files[tools::file_path_sans_ext(files) %in% app_files]
		for(app_file in files) {
			app_source <- scan(file = paste0(build_dir, app_name, '/', app_file),
							   what = character(),
							   quiet = TRUE,
							   sep = '\n') |>
				paste0(collapse = '\n')
			# ToDo: Add data

			# Add function calls
			visualstats_funs <- stringr::str_match(app_source, "VisualStats::*(.*?)\\s*\\(") |>
				as.data.frame() |>
				tidyr::drop_na()
			if(nrow(visualstats_funs) > 0) {
				visualstats_funs <- unique(visualstats_funs[,2,drop=TRUE])
			} else {
				visualstats_funs <- character()
			}
			for(fun_name in visualstats_funs) {
				fun_source <- get(fun_name) |> deparse() |> paste0(collapse = '\n')
				app_source <- paste0(
					fun_name, ' <- ', fun_source, '\n\n', app_source
				)
			}
			app_source <- sub('VisualStats::', '', app_source)

			# Add dependencies
			# app_source <- paste0('\n', pkgs, '\n', app_source)

			cat(app_source, file = paste0(build_dir, app_name, '/', basename(app_file)))
		}

		# shinylive::export(appdir = paste0(build_dir, app_name),
		# 				  destdir = paste0(out_dir, '/', app_name))
		shinylive::export(appdir = paste0(build_dir, app_name),
						  destdir = out_dir,
						  subdir = app_name)
	} # else Not a shiny application
}

# shinylive::export(appdir = 'inst/shiny/variance/',
# 				  destdir = 'docs/shiny/variance/')
# httpuv::runStaticServer("docs/shiny/variance/")

app_index <- paste0(
	'<html><head><title>Shiny Applications</title></head><body><ul>\n',
	app_index,
	'\n</ul></body></html>'
)
# cat(app_index, file = paste0(out_dir, '/index.html'))

httpuv::runStaticServer(dir = out_dir, port = 2112)

unlink(build_dir, recursive = TRUE) # Cleanup

##### Tests
# Create a test
# usethis::use_test('loess-test')

##### Check exported functions and data
ls('package:VisualStats')
data(package = 'VisualStats')

data("hand_washing")
anova_vis(hand_washing$Bacterial_Counts, hand_washing$Method)
anova_vis(hand_washing$Bacterial_Counts, hand_washing$Method,
		  plot_boxplot = TRUE)

##### Shiny Apps
shiny_demo()
shiny_demo('variance')
shiny_demo('anova')
shiny_demo('loess')
