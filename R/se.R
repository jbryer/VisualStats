#' Calculate standard error
#'
#' If `x` is a numeric vector with more than two unique values, this returns the standard error of
#' the mean using the following formula:
#'
#' \deqn{se = \frac{s}{\sqrt{n}}}
#'
#' where *s* is the sample standard deviation and *n* is the sample size.
#'
#' If `proportion = TRUE` (which is the default when there are only two unique values in `x`) this
#' returns the standard error for a proportion using the following formula:
#'
#' \deqn{se = \frac{p\(1 - p)}{n}}
#'
#' where *p* is the proportion of success and *n* is the sample size.
#'
#' @param x a numeric vector to calculate the standard error for.
#' @param proportion whether to use the standard error for a proportion.
#' @param na.rm a logical evaluating to `TRUE` or `FALSE` indicating whether `NA` values should be
#'        stripped before the computation proceeds.
#' @return the standard error.
#' @export
se <- function(x, proportion = length(unique(x)) == 2, na.rm = TRUE) {
	result <- NA
	if(na.rm) {
		x <- x[!is.na(x)]
	}
	if(proportion) {
		p_hat <- unname(prop.table(table(x))[1])
		result <- sqrt( (p_hat * (1 - p_hat)) / length(x) )
		names(result) <- 'Standard error for proportion'
	} else {
		if(!is.numeric(x)) {
			stop('x must be a numeric vector.')
		}
		result <- sd(x) / sqrt(length(x))
		names(result) <- 'Standard error for mean'
	}
	return(result)
}
