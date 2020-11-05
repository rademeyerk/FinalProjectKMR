#' The Mixture Normal Distribution
#'
#' This function allows you to sample from a mixture normal distribution with
#' user-inputted probabilities, means, and standard deviations
#'
#' @param p is the probability of sampling from the first normal distribution
#' @param mu1 and @param sigma1 are the parameters of this first distribution,
#' and @param mu2 / @param sigma2 are the parameters of the second distribution,
#' which has a probability of being sampled of 1-p.
#'
#' @export
#' @examples MixtureNormal(0.5, 5, 5, 1, 10)
#' #' You need the suggested package for this function


MixtureNormal <- function(p, mu1, mu2, sigma1, sigma2) {
  U <- purrr::rbernoulli(1, p = p)
  ifelse(U == 1, rnorm(1, mean = mu1, sd = sigma1), rnorm(1, mean = mu2, sd = sigma2))
}
