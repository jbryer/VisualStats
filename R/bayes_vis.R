#' Visualization for Bayes Formula
#'
#' \deqn{
#' P(A | B) = \frac{P(B|A) \items P(A)}{P(B)}
#' }
#'
#' where
#'
#' \deqn{
#' P(B) = P(A) \times P(B | A) + P(not A) \times P(not A | A)
#' }
#'
#' @export
#' @examples
#' # Adapted from Tversky & Kaheman (1974) https://www.jstor.org/stable/1738360
#' p_a <- 1/21
#' p_not_a <- 1 - p_a
bayes_vis <- function() {
}
