#' Warpper to optim to save iterations.
#'
#' This function wraps the \link{stats::optim} function and saves the parameters
#' and likelihood estimation at each step of the algorithm.
#'
#' @param par initial parameters to to be optimized over.
#' @param fn the function to minimized (or maximized).
#' @param ... other parameters passed to optim.
#' @return the results of optim with two additional elements, iterations with a
#'         a list of the values at each iteration of the algorithm and
#'         iterations_df which is a data.frame version of the list.
#' @seealso stats::optim
#' @export
optim_save <- function(par, fn, ...) {
	iterations <- list()
	wrap_fun <- function(parameters, ...) {
		n <- length(iterations)
		result <- fn(parameters, ...)
		iterations[[n + 1]] <<- c(parameters, result)
		return(result)
	}
	optim_out <- stats::optim(par, wrap_fun, ...)
	optim_out$iterations <- iterations
	optim_out$iterations_df <- as.data.frame(do.call(rbind, iterations))
	names(optim_out$iterations_df) <- c(paste0('Param', 1:length(par)), 'Result')
	optim_out$iterations_df$Iteration <- 1:nrow(optim_out$iterations_df)
	return(optim_out)
}
