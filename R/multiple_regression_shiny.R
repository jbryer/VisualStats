#' Run the Shiny server for visualizing multiple regression
#'
#' This Shiny application provides an interactive interface to the [multiple_regression_vis()] function.
#'
#' @param ... other parameters passed to [shiny::shinyApp()]
#' @return results of shinyApp
#' @export
#' @rdname multiple_regression_shiny
multiple_regression_shiny <- function(...) {
	# TODO: move the shiny app into the package as functions
	app_path <- file.path(find.package('VisualStats'), 'shiny', 'multiple_regression')
	shiny::runApp(appDir = app_path)
}
