#' @title Get Discount
#' @description Calculates the discount rate at each time point in a Markov chain.
#' @param disc_rate The discount rate at time \emph{0}.
#' @param t A vector of time points in the Markov chain.
#' @details \emph{disc_rate} and \emph{t} must be in the same time units (i.e. discount rate = 1.5% per annum, t is in years).
#' @return A vector of discount rates for each time point in a Markov chain. A \emph{NULL} value is returned if \emph{disc_rate} or any value in \emph{t} is less than 0.
#' @export 
GetDiscount <- function(disc_rate, t){
  
  if(disc_rate<0){
    print("ERROR (GetDiscount): Passed a discount rate less than 0. Returning NULL.")
    return()
  } 
  
  if(any(t<0)){
    print("ERROR (GetDiscount): Passed a time value less than 0. Returning NULL.")
    return()
  } 
  
  return(1/((1+disc_rate)^seq(0,t-1)))
  
}

#' @title Continuous discount function
#' @description Continuous discounting function
#' @param discRate Discount rate
#' @param t1 First time point
#' @param t2 Second time point
#' @return Discount factor
#' @export
getContinuousPayoff <- function(discRate, t1, t2) {
  inst_dr = log(1+discRate);
  
  return((exp(-inst_dr * t2) - exp(-inst_dr * t1)) / -inst_dr);
}