#' @title Get Discount
#' @description Calculates the discount rate at each time point in a Markov chain.
#' @param disc_rate The discount rate at time \emph{0}.
#' @param t A vector of time points in the Markov chain.
#' @details \emph{disc_rate} and \emph{t} must be in the same time units (i.e. discount rate = 1.5% per annum, t is in years).
#' @return A vector of discount rates for each time point in a Markov chain.
GetDiscount <- function(disc_rate, t){
  return(1/((1+disc_rate)^seq(0,t-1)))
}
