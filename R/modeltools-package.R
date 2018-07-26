#' Decision Modelling Tools
#' 
#' @description 
#' A collection of functions to handle tasks commonly used in developing decision models.
#' \subsection{Probabilities and Rates}{
#'   \code{\link{ProbToRate}} converts a cumulative probability over a given time frame to a rate.\cr\cr
#'   \code{\link{RateToProb}} converts a rate to a cumulative probability over a given time frame.\cr\cr
#'   \code{\link{ProbToOdds}} converts a probability to an odds.\cr\cr
#'   \code{\link{OddsToProb}} converts an odds to a probability.
#' }
#' 
#' \subsection{Distributions}{
#'   \code{\link{estBetaParam}} estimates parameters for a beta distribution given a mean and standard deviation
#' using method of moments.\cr\cr
#'   \code{\link{estGammaParam}} estimates parameters for a gamma distribution given a mean and standard deviation
#' using method of moments.\cr\cr
#'   \code{\link{estLogNormParam}} estimates parameters for a lognormal distribution given one of: 
#'   \itemize{
#'   \item mean and standard deviation, or
#'   \item mean and lower/upper bounds of a 95\% confidence interval.
#'   }
#' }
#' \subsection{Other}{
#'   \code{\link{GetDiscount}} returns an instantaneous discount rate for a vector of time points. 
#' }
#' 
#' @docType package
#' @name modeltools
#' @author Austin Nam (\email{austin.nam@@mail.utoronto.ca})
NULL