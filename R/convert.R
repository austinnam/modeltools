#' @title Convert a probability to a rate
#' @description Converts a vector of probabilities to a vector of rates. 
#' @param p A vector of probabilities.
#' @param t The time frame of the probability. 
#' @return A vector of rates in the unit of time \emph{t}. If any probabilities are less than 0 or greater than 1, the function returns \emph{NULL}. 
#' @examples 
#' # For a cumulative probability of 50% over 2 years
#' ProbToRate(0.5,2) # returns an annual rate of 0.35
#' ProbToRate(0.5,24) # returns a monthly rate of 0.02 
#' @export
ProbToRate <- function(p, t=1){
  
  # exception handling for prob<0 or prob > 1
  if(any(p<0)){
    print("ERROR (ProbToRate): Passed probability less than 0. Returning NULL.")
    return()
  } 
  
  if(any(p>1)){
    print("ERROR (ProbToRate): Passed probability greater than 1. Returning NULL.")
    return()
  } 
  
  return(-log(1-p)/t)
  
}


#' @title Convert a rate to a probability
#' @description Converts a vector of rates to a vector of probabilities. 
#' @param r A vector of rates.
#' @param t The time frame of the desired probability in the units of the input rate.
#' @return A vector of probabilities in the unit of time \emph{t}. If any rates are less than 0, the function returns \emph{NULL}.
#' @examples 
#' # If event rate over over 2 years is 50%
#' RateToProb(0.5, 1/2) # returns an annual probability of 0.22
#' RateToProb(0.5, 1/24) # returns a monthly probability of 0.02
#' @export
RateToProb <- function(r, t=1){
  
  # exception handling for rates less than 0
  if(any(r<0)) {
    print("ERROR (RateToProb): Passed a rate less than 0. Returning NULL.") 
    return()
  }
  
  return(1-exp(-r*t))
}

#' @title Convert probability to odds
#' @description Converts a vector of probabilities to odds. 
#' @param p A vector of probabilities.
#' @return A vector of odds. If any probabilities are less than 0 or greater than 1, the function returns a \emph{NULL}.
#' @export
ProbToOdds <- function(p){
  
  # exception handling if prob < 0 or prob > 1
  if(any(p<0)){
    print("ERROR (ProbToOdds): Passsed a probability less than 0. Returning NULL.")
    return()
  } 
  if(any(p>1)){
    print("ERROR (ProbToOdds): Passed a probability greater than 1. Returning NULL.")
    return()
  } 

  return(p/(1-p))
  
}

#' @title Convert odds to probability
#' @description Converts a vector of odds to probabilities. 
#' @param o A vector of odds.
#' @return A vector of probabilities. If any odds are less than 0, the function returns \emph{NULL}.
#' @export
OddsToProb <- function(o){
  
  # exception if odds < 0
  if(any(o<0)){
    print("ERROR (OddsToProb): Passed an odds less than 0. Returning NULL.")
    return()
  } 
  
  return(o/(1+o))

}

#' @title Round to Specified Multiple
#' @description Round a vector to the nearest user-specified multiple.
#' @param v A vector of values to be rounded.
#' @param m A multiple to round values to.
#' @return A vector of values rounded to the nearest multiple, \emph{m}.
#' @export
roundToMult <- function(v,m){
  
  # check arguments
  if(length(v)==0) stop("Supplied an empty vector.")
  if(length(m)==0) stop("Did not supply a multiple to round to.")
  if(length(m)>1){
    print("Supplied m of length > 1. Using first element only.")
    m <- m[1]
  }
  
  return(round(v/m, digits=0)*m)
}
