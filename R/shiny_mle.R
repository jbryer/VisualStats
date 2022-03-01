#' Visualizing Maximum Likelihood Estimation Shiny Application
#'
#' This will start a shiny app explore maximum likelihood estimation visually.
#'
#' @references http://rstudio.com/shiny
#' @export
shiny_mle <- function(df, default_x, default_y, ...) {
	shiny_env <- new.env()

	if(!missing(df)) {
		assign('thedata', df, shiny_env)
	}
	if(!missing(default_x)) {
		assign('default_x', default_x, shiny_env)
	}
	if(!missing(default_y)) {
		assign('default_y', default_y, shiny_env)
	}

	environment(shiny_ui) <- shiny_env
	environment(shiny_server) <- shiny_env

	app <- shiny::shinyApp(
		ui = shiny_ui,
		server = shiny_server
	)

	shiny::runApp(app, ...)
}


