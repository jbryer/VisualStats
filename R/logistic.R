#' Logistic Function
#'
#' @param x vector of values.
#' @param beta0 intercept.
#' @param beta1 slope.
#' @export
logistic <- function(x, beta0, beta1) {
	return(1 / (1 + exp(-1 * (beta0 + beta1 * x)) ))
}

#' Logit Function
#'
#' @param x vector of values.
#' @param beta0 intercept.
#' @param beta1 slope.
#' @export
logit <- function(x, beta0, beta1) {
	return( 1 / (1 + exp(-beta0 - beta1 * x)) )
}
