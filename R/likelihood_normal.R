#' Calculate the log likelihood for a normally distributed dependent variable.
#'
#' @param parameters the regression parameters. Element one is the intercept,
#'        element two is the slope, and element three is the error (root mean
#'        square error).
#' @param predictor vector of predictors.
#' @param outcome vector of outcomes.
#' @return the log likelihood.
#' @export
loglikelihood_normal <- function(parameters, predictor, outcome) {
	a <- parameters[1] # Intercept
	b <- parameters[2] # beta coefficient
	sigma <- parameters[3] # error
	# Calculate the likelihood of y given a + b = x
	ll.vec <- dnorm(outcome, a + b * predictor, sigma, log = TRUE)
	# sum the likelihood over all the values in the data
	return(sum(ll.vec))
}
