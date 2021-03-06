% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/payoff.R
\name{GetDiscount}
\alias{GetDiscount}
\title{Get Discount}
\usage{
GetDiscount(disc_rate, t)
}
\arguments{
\item{disc_rate}{The discount rate at time \emph{0}.}

\item{t}{A vector of time points in the Markov chain.}
}
\value{
A vector of discount rates for each time point in a Markov chain. A \emph{NULL} value is returned if \emph{disc_rate} or any value in \emph{t} is less than 0.
}
\description{
Calculates the discount rate at each time point in a Markov chain.
}
\details{
\emph{disc_rate} and \emph{t} must be in the same time units (i.e. discount rate = 1.5% per annum, t is in years).
}
