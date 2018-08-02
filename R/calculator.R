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