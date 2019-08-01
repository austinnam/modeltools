#' @title Random Weibull 
#' @description Retrieves random variate from weibull model.
#' @param n The number of variates to sample.
#' @param b A vector of weibull regression coefficients.
#' @param x A vector of covariate values.
#' @param s Shape parameter.
#' @return A vector of times to event. 
#' @export
rweireg <- function(n,b,x,s){
  # only take the first value of n and s
  n <- n[1]
  s <- s[1]
  a <- exp(sum(b*x))
  t <- exp(log(-log(runif(n)))/s)/a
  return(t)
}