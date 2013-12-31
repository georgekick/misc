#' function template
#'  
#' @param fun a string of a function name
#' @param folder a relative reference path to a destination folder (leave it empty for the current directory)
#' @return fun.R
#' @description create a function template in the working directory

funTemp <- function(fun="fun", folder="/R"){
  if (length(folder)>0){
    filename <- file(paste(getwd(), folder, "/", fun, ".R", sep=""), "w",  blocking=FALSE)
  } else {
    filename <- file(paste(fun, ".R", sep=""), "w",  blocking=FALSE)
  }
  write(paste("#\' function: ", fun, sep=""), filename)
  write("#\' @param <name> <description>", filename, append = TRUE)
  write("#\' @param <name> <description>", filename, append = TRUE)
  write("#\' @return <description>", filename, append = TRUE)
  write("#\' @export (*delete* exports function (no documentation))", filename, append = TRUE)
  write("#\' @examples <Rcode>", filename, append = TRUE)
  write("#\' \\donttest{", filename, append= TRUE)
  write("#\' ", filename, append= TRUE)  
  write("#\' }", filename, append= TRUE)
  write("#\' @example <path to R file>", filename, append = TRUE)
  write("#\' @note <contents>", filename, append = TRUE)
  write("#\' @section <name> : <contents>", filename, append = TRUE)
  write("#\' @references <references>", filename, append = TRUE)
  write("", filename, append = TRUE)
  write(paste(fun, "<- function(){", sep=""), filename, append = TRUE)
  write("", filename, append = TRUE)
  write("", filename, append = TRUE)
  write("}", filename, append = TRUE)
  close(filename)
}