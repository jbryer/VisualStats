#' Calculate standard error
#'
#' Calculates the standard error of the mean using the following formula:
#'
#' \deqn{se = \frac{s}{\sqrt{n}}}
#'
#' where *s* is the sample standard deviation and *n* is the sample size.
#'
#' @param x a numeric vector to calculate the standard error for.
#' @return the standard error.
#' @export
se <- function(x) {
	stopifnot(is.numeric(x))
	sd(x) / sqrt(length(x))
}
