#' Check if a vector is dichotomous.
#'
#' This function checks to see if a vector is dichotomous. This includes
#' logical vectors and numeric vectors that contain only 0s and 1s.
#'
#' @param x vector to check.
#' @return TRUE if the vector is binary.
#' @export
isBinary <- function(x) {
	return(is.logical(x) | all(x %in% c(0,1)))
}
