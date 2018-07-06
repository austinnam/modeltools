#' @title Convert a probability to a rate
#' @description Converts a vector of probabilities to a vector of rates. 
#' @param p A vector of probabilities.
#' @param t The time frame of the probability. 
#' @details If any probabilities are less than 0 or greater than 1, the function returns \emph{NULL}. 
#' @return A vector of rates in the unit of time \emph{t}.
#' @examples 
#' # For a cumulative probability of 50% over 2 years
#' ProbToRate(0.5,2) # returns an annual rate of 0.35
#' ProbToRate(0.5,24) # returns a monthly rate of 0.02 
ProbToRate <- function(p, t=1){
  # Exception handling for probabilities outside (0,1)
  if(any(p<0)){
    print("ERROR (ProbToRate): Passed probability less than 0. Returning NULL.")
    return()
  }
  else if(any(p>1)){
    print("ERROR (ProbToRate): Passed probability greater than 1. Returning NULL.")
    return()
  }
  
  else return(-log(1-p)/t)
}


#' @title Convert a rate to a probability
#' @description Converts a vector of rates to a vector of probabilities. 
#' @details If any rates are less than 0, the function returns \emph{NULL}.
#' @param r A vector of rates.
#' @param t The time frame of the desired probability in the units of the input rate.
#' @return A vector of probabilities in the unit of time \emph{t}.
#' @examples 
#' # If event rate over over 2 years is 50%
#' RateToProb(0.5, 1/2) # returns an annual probability of 0.22
#' RateToProb(0.5, 1/24) # returns a monthly probability of 0.02
RateToProb <- function(r, t=1){
  # Exception handling for rates < 0.
  if(any(r<0)){
    print("ERROR (RateToProb): Passed a rate less than 0. Returning NULL.")
    return()
  }
  
  else return(1-exp(-r*t))
}

#' @title Convert probability to odds
#' @description Converts a vector of probabilities to odds. 
#' @details If any probabilities are less than 0 or greater than 1, the function returns a \emph{NULL}.
#' @param p A vector of probabilities.
#' @return A vector of odds.
ProbToOdds <- function(p){
  # Exception handling for probabilities outside (0,1)
  if(any(p<0)){
    print("ERROR (ProbToOdds): Passsed a probability less than 0. Returning NULL.")
    return()
  }
  else if(any(p>1)){
    print("ERROR (ProbToOdds): Passed a probability greater than 1. Returning NULL.")
    return()
  }
  
  else return(p/(1-p))
}

#' @title Convert odds to probability
#' @description Converts a vector of odds to probabilities. 
#' @details If any odds are less than 0, the function returns \emph{NULL}.
#' @param o A vector of odds.
#' @return A vector of probabilities.
OddsToProb <- function(o){
  # Exception handling for any odds < 0.
  if(any(o<0)){
    print("ERROR (OddsToProb): Passed an odds less than 0. Returning NULL.")
    return()
  }
  
  else return(o/(1+o))
  
}