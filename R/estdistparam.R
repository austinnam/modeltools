

#######################################################################################################

####                    ESTIMATING PARAMETERS OF PARAMETRIC DISTRIBUTIONS

#######################################################################################################




#' @title Estimate Gamma Distribution Parameters
#' @description Estimate parameters of a gamma distribution using method of moments.
#' @param mu Mean of the gamma distribution.
#' @param sigma Standard deviation of the gamma distribution.
#' @references https://www.rocscience.com/help/swedge/webhelp/swedge/Gamma_Distribution.htm
#' 
#' @details \eqn{\alpha} (shape) and \eqn{\beta} (scale) parameters are estimated as:
#' \deqn{\alpha = (mu/sigma)^2}
#' \deqn{\beta = (sigma^2)/mu} 
estGammaParam <- function(mu, sigma){
  
  alpha <- (mu/sigma)^2
  beta <- sigma^2/mu
  
  return(list(alpha=alpha,beta=beta))
  
}



#' @title Estimate Beta Distribution Parameters
#' @description Estimate parameters of a beta distribution using method of moments.
#' @param mu Mean of the beta distribution.
#' @param sigma Standard deviation of the beta distribution.
#' @references 
#' https://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
#' @details 
#' Beta(\eqn{\alpha},\eqn{\beta}) distribution has two shape parameters, \eqn{\alpha} and \eqn{\beta}, estimated as:
#' \deqn{\alpha = [(1-mu)/sigma^2 - 1/mu] * mu^2}
#' \deqn{\beta = alpha * (1/mu - 1)}
estBetaParam <- function(mu, sigma){
  
  alpha <- ((1-mu)/sigma^2 - 1/mu) * mu^2
  beta <- alpha * (1/mu - 1)
  
  return(list(alpha=alpha, beta=beta))
  
}



#' @title Estimate Lognormal Distribution
#' @description Estimate parameters of a lognormal distribution using method of moments.
#' @param mu Mean of estimate.
#' @param sigma Standard deviation of estimate.
#' @param lower Lower limit of 95\% confidence interval of estimate.
#' @param upper Upper limit of 95\% confidence interval of estimate.
#' @return Log-mean and Log-sd of the lognormal distribution.
#' @references 
#' https://en.wikipedia.org/wiki/Log-normal_distribution
#' @details 
#' Parameters for a 95\% confidence interval are optional if mu and sigma are provided. 
#' If neither sigma nor 95\% confidence interval are provided, the function returns \emph{NULL}.
#' 
#' Where \emph{mu} and \emph{sigma} are provided, parameters are estimated by:
#' \deqn{log(\mu) = log(mu) - log(\sqrt{1 + sigma^2/mu^2})}
#' \deqn{log(\sigma) = \sqrt{log(1 + sigma^2/mu^2)}}
#' 
#' Where \emph{mu} and a 95\% confidence interval are provided, parameters are estimated by:
#' \deqn{log(\mu) = log(mu)}
#' \deqn{log(\sigma) = [(log(upper) - log(lower))/(2*1.959964)]}

estLogNormParam <- function(mu, sigma=NULL, lower=NULL, upper=NULL){
  
  if(!is.null(sigma)){
    logn.mu <- log(mu)-log(sqrt(1 + sigma^2/mu^2))
    logn.sd <- sqrt(log(1 + sigma^2/mu^2))
  } else if(!is.null(lower) & !is.null(upper)){
    logn.mu <- log(mu)
    logn.sd <- ((log(upper)-log(lower))/(2*qnorm(0.975)))
  } else {
    return()
  }
  
  return(list(mu=logn.mu, sigma=logn.sd))
}





