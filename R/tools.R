#' @title Confidence interval of a vector 
#' @description Calculates confidence interval of a vector using a normal distribution
#' @param x A numeric vector 
#' @param conf.level confidence level of the vector
#' @return A vector of the lower/upper limits of the confidence interval
#' @export
confintVector <- function(x, conf.level=0.95){
  if(conf.level>=1 | conf.level<=0){
    print("Confidence interval must be between 0 and 1.")
    return()
  }
  
  mu <- mean(v)
  sigma <- sd(v)
  alpha <- 1-conf.level
  error <- qnorm(1-alpha/2)*sigma/sqrt(length(x))
  
  return(c(mu-error, mu+error))
  
}


#' @title Fieller's Confidence Interval 
#' @description Calculates confidence interval of a ratio of vectors under a normal distribution
#' @param x A numeric vector of the numerator
#' @param y A numeric vector of the denominator
#' @param conf.level confidence level of the vector
#' @param round number of decimal places to use
#' @return A vector of the lower/upper limits of the confidence interval
#' @export
fieller <- function(x,y,conf.level=0.95, round = 2) {
  if(conf.level>=1 | conf.level<=0){
    print("Confidence level must be between 0 and 1.")
    return()
  }
  alpha <- 1-conf.level
  x.mean <- mean(x)
  y.mean <- mean(y)
  x.sd <- sd(x)
  y.sd <- sd(y)
  xy.cov <- cov(x,y)
  
  print(sprintf("x has mean %s, y has mean %s", round(x.mean, digits=round), round(y.mean, digits=round)))
  print(sprintf("ratio of means is %s", round(x.mean/y.mean, digits=round)))
  
  g <- pt(alpha,length(y) - 1)^2*(y.sd^2)/(y.mean^2)
  
  lower <- (1 / (1-g)) * ((x.mean/y.mean) - g*xy.cov/(y.sd^2) - pt(alpha, length(y) - 1)/y.mean * sqrt (x.sd^2 - 2*x.mean/y.mean*xy.cov + x.mean^2/y.mean^2 * y.sd^2 - g*(x.sd^2 - xy.cov^2/y.sd^2)))
  upper <- (1 / (1-g)) * ((x.mean/y.mean) - g*xy.cov/(y.sd^2) + pt(alpha, length(y) - 1)/y.mean * sqrt (x.sd^2 - 2*x.mean/y.mean*xy.cov + x.mean^2/y.mean^2 * y.sd^2 - g*(x.sd^2 - xy.cov^2/y.sd^2)))
  return(round(c(lower, upper), digits=round))
}