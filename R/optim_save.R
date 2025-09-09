#' Warpper to optim to save iterations.
#'
#' This function wraps the [stats::optim()] function and saves the parameters
#' and likelihood estimation at each step of the algorithm.
#'
#' @rdname optim-save
#' @param par initial parameters to to be optimized over.
#' @param fn the function to minimized (or maximized).
#' @param ... other parameters passed to optim.
#' @return the results of optim with two additional elements, iterations with a
#'         a list of the values at each iteration of the algorithm and
#'         iterations_df which is a data.frame version of the list.
#' @importFrom stats optim
#' @export
#' @examples
#' data("mtcars")
#' residual_sum_squares <- function(parameters, predictors, outcome) {
#'     if(length(parameters) - 1 != ncol(predictors)) {
#'   	   stop('Number of parameters does not match the number of predictors.')
#'     }
#'     predicted <- 0
#'     for(i in 1:ncol(predictors)) {
#'   	   predicted <- predicted + parameters[i] * predictors[i]
#'     }
#'     predicted <- predicted + parameters[length(parameters)]
#'     residuals <- outcome - predicted
#'     ss <- sum(residuals^2)
#'     return(ss)
#' }
#' optim.rss <- optim_save(
#'     par = runif(3),
#'     fn = residual_sum_squares,
#'     predictors = mtcars[,c('wt', 'vs')],
#'     outcome = mtcars$mpg
#' )
#' optim.rss
#' plot(optim.rss)
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
	class(optim_out) <- c('optim', 'list')
	return(optim_out)
}

#' Print method for `optim_save`
#'
#' @rdname optim-save
#' @param x result from [option_save()].
#' @param ... currently unused.
#' @return the final parameters.
#' @method print optim
#' @export
print.optim <- function(x, ...) {
	print(x$par)
}

#' Summary method for `optim_save`
#'
#' @rdname optim-save
#' @param object result from [optim_save()].
#' @param digits number of digits to round the parmaeters to.
#' @param ... currently unused.
#' @return `object` invisibly.
#' @method summary optim
#' @export
summary.optim <- function(object, digits = 3, ...) {
	cat(
		length(object$par), 'parameters estimated in',
		nrow(object$iterations_df), 'iterations.\nFinal parameter estimates:',
		paste0(round(object$par, digits = digits), collapse = ', ')
	)
	invisible(object)
}

#' Plot method for `optim_save`
#'
#' @rdname optim-save
#' @param x result from [option_save()].
#' @param param_labels character vector to label the parameters (e.g. variable names).
#' @param result_label character to label the result (i.e. the resutl of function optimized).
#' @param ... currently unused.
#' @return a ggplot2 expression.
#' @method plot optim
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
plot.optim <- function(x, param_labels, result_label, ...) {
	df <- x$iterations_df
	n_params <- length(x$par)
	if(!missing(param_labels)) {
		if(length(param_labels) != n_params) {
			stop(paste0('param_labels is not equal to the number of parameters estimated: ', n_params))
		}
		names(df)[1:n_params] <- param_labels
	}
	if(!missing(result_label)) {
		names(df)[(n_params + 1)] <- result_label
	}
	df |>
		reshape2::melt(id.var = 'Iteration') |>
		ggplot(aes(x = Iteration, y = value, color = variable)) +
		geom_point(size = 1) + geom_path() +
		facet_wrap(~ variable, scales = "free_y", ncol = 1) +
		xlab('Iteration') + ylab('') +
		theme_vs() + theme(legend.position = 'none')
}
