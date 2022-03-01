#' Calculate the log likelihood for a binomial distribution.
#'
#' @param parameters the regression parameters. Element one is the intercept,
#'        element two is the slope or beta coefficient.
#' @param predictor vector of predictors.
#' @param outcome vector of outcomes.
#' @return the log likelihood.
#' @export
loglikelihood_binomial <- function(parameters, predictor, outcome) {
	a <- parameters[1] # Intercept
	b <- parameters[2] # beta coefficient
	p <- logit(predictor, a, b)
	ll <- sum( outcome * log(p) + (1 - outcome) * log(1 - p))
	return(ll)
}
