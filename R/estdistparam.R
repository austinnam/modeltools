# ESTIMATING PARAMETERS OF PARAMETRIC DISTRIBUTIONS


# Gamma Distribution --------------------------------

#' @title Estimate Gamma Distribution Parameters
#' @description Estimate parameters of a gamma distribution using method of moments.
#' @param mu Mean of the gamma distribution. 
#' \emph{mu} cannot have a negative value.
#' @param sigma Standard deviation of the gamma distribution.
#' \emph{sigma} cannot have a negative value.
#' @references https://www.rocscience.com/help/swedge/webhelp/swedge/Gamma_Distribution.htm
#' 
#' @details \eqn{\alpha} (shape) and \eqn{\beta} (scale) parameters are estimated as:
#' \deqn{\alpha = (mu/sigma)^2}
#' \deqn{\beta = (sigma^2)/mu} 
#' @export
estGammaParam <- function(mu, sigma){
  
  tryCatch(
    {
      alpha <- (mu/sigma)^2
      beta <- sigma^2/mu
      return(list(alpha=alpha,beta=beta))
    },
    if(any(mu<0)) print("ERROR (estGammaParam): mu cannot be negative. Returning NULL."),
    if(any(sigma<0)) print("ERROR (estGammaParam): sigma cannot be negative. Returning NULL."),
    return() # return NULL if any exception occurs
  )

}


# Beta Distribution ------------------------------

#' @title Estimate Beta Distribution Parameters
#' @description Estimate parameters of a beta distribution using method of moments.
#' @param mu Mean of the beta distribution. 
#' \emph{mu} must be between 0 and 1.
#' @param sigma Standard deviation of the beta distribution.
#' \emph{sigma} cannot have a negative value.
#' @references 
#' https://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
#' @details 
#' Beta(\eqn{\alpha},\eqn{\beta}) distribution has two shape parameters, \eqn{\alpha} and \eqn{\beta}, estimated as:
#' \deqn{\alpha = [(1-mu)/sigma^2 - 1/mu] * mu^2}
#' \deqn{\beta = alpha * (1/mu - 1)}
#' @export
estBetaParam <- function(mu, sigma){
  
  tryCatch(
    {
      alpha <- ((1-mu)/sigma^2 - 1/mu) * mu^2
      beta <- alpha * (1/mu - 1)
      return(list(alpha=alpha, beta=beta))
    },
    if(any(mu<0)) print("ERROR (estBetaParam): mu must not be less than 0. Returning NULL."),
    if(any(mu>1)) print("ERROR (estBetaParam): mu must not be greater than 1. Returning NULL."),
    if(any(sigma<0)) print("ERROR (estBetaParam): sigma must not be negative. Returning NULL."),
    return() # return NULL if any exception occurs
  )

}



#' @title Estimate Lognormal Distribution
#' @description Estimate parameters of a lognormal distribution using method of moments.
#' @param mu Mean of estimate.
#' \emph{mu} cannot have a negative value and must be of length=1. 
#' @param s Standard deviation or 95\% confidence interval of estimate.
#' \emph{s} cannot have a negative value. 
#' @return Log-mean and Log-sd of the lognormal distribution.
#' @references 
#' https://en.wikipedia.org/wiki/Log-normal_distribution
#' @details 
#' \emph{s} can only contain 1 or 2 elements. 
#' If 1 element is supplied, then s is used as the standard deviation. 
#' If 2 elements are supplied, then s is used as a 95\% confidence interval 
#' with the first element as the lower bound. \cr
#' This function is not vectorised. If s has more than 2 elements, the function returns NULL.
#' If mu has more than 1 element, then only the first element is used. 
#' 
#' If neither sigma nor 95\% confidence interval are provided, the function returns \emph{NULL}.
#' 
#' Where a standard deviation is provided, parameters are estimated by:
#' \deqn{log(\mu) = log(mu) - log(\sqrt{1 + s^2/mu^2})}
#' \deqn{log(\sigma) = \sqrt{log(1 + s^2/mu^2)}}
#' 
#' Where a 95\% confidence interval are provided, parameters are estimated by:
#' \deqn{log(\mu) = log(mu)}
#' \deqn{log(\sigma) = [(log(s[2]) - log(s[1]))/(2*1.959964)]}
#' @export
estLogNormParam <- function(mu, s){
  
  # ensure user supplies only 1 or 2 values for s
  if(length(s)>2){
    print("ERROR (estLogNormParam): s must be length 1 or 2. Supply either a standard deviation or lower/upper bounds of a 95% confidence interval.")
    return()
  }
  # ensure mu is only 1 value
  # this function is not vectorised
  if(length(mu)>1){
    print("WARNING (estLogNormParam): supplied more than 1 value for mu. Using first value only.")
    mu <- mu[1]
  }
  
  # mu must be positive
  if(mu<0){
    print("ERROR (estLogNormParam): mu must not be negative. Returning NULL.")
    return()
  }
  # s must be positive
  if(any(s<0)){
    print("ERROR (estLogNormParam): s cannot contain negative values. Returning NULL.")
    return()
  }
  
  # if s contains only 1 element, then assume user is supplying the standard deviation
  if(length(s)==1){
    logn.mu <- log(mu)-log(sqrt(1 + s^2/mu^2))
    logn.sd <- sqrt(log(1 + s^2/mu^2))
  }
  
  # if s contains 2 elements, then assume user is supplying a 95% confidence interval
  if(length(s)==2){
    logn.mu <- log(mu)
    logn.sd <- ((log(s[2])-log(s[1]))/(2*qnorm(0.975)))
  }

  return(list(logmu=logn.mu, logsigma=logn.sd))
}





