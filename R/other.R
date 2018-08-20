#' @title Convert Windows Filepath to Character String for R
#' @description To help copy/paste a filepath in Windows containing backslashes ('\')
#' that can be passed to R functions. 
#' @return A converted character string with backslashes converted to forward slash. 
#' @export
convertFilepath <- function(){
  input <- readline(prompt = "Enter filepath: ")
  output <- gsub("\\\\", "/", input)
  return(output)
}
