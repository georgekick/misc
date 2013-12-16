#' analysis template
#'  
#' @param file a string of a file name
#' @return file.R
#' @description create a analysis file template in the working directory


### needs to be changed

funTemp <- function(file="file"){
  filename <- file(paste(file, ".R", sep=""), "w",  blocking=FALSE)
  write(paste("#\' function: ", fun, sep=""), filename)
  write("#####################", filename, append = TRUE)
  write("# purposes...                   ", filename, append = TRUE)
  write(paste("#Date:", Sys.Date(), sep=""), filename, append = TRUE)
  write("#####################", filename, append = TRUE)
  write("", filename, append = TRUE)
  
  write("### load libraries", filename, append = TRUE)
  write("library(rgdal)", filename, append = TRUE)
  write("library(ggplot2)", filename, append = TRUE)
  
  write("### set working directory", filename, append = TRUE)
  write("# setwd(\"R\")", filename, append = TRUE)
  
  write("### read data", filename, append = TRUE)
  write("data <- read.table(\"data/data.csv\"), header=TRUE, sep=\",\")", filename, append = TRUE)
  
  write("# read xls", filename, append = TRUE)
  write("library(RODBC)", filename, append = TRUE)
  write("channel <- odbcConnectExcel(\"c:/myexel.xls\")", filename, append = TRUE)
  write("data <- sqlFetch(channel, \"Sheet1\")", filename, append = TRUE)
  write("odbcClose(channel)", filename, append = TRUE)
  
  write("# read xlsx", filename, append = TRUE)
  write("library(xlsx)", filename, append = TRUE)
  write("data <- read.xlsx(\"MyExcelFile.xlsx\", 1 , stringsAsFactors=F)", filename, append=TRUE)
    
  write("# read spatial data", filename, append = TRUE)
  write("spdf <- readOGR(\"shapefile.shp\", layer=\"shapefile\")", filename, append = TRUE)
  
  write("### analysis", filename, append = TRUE)
  write("", filename, append = TRUE)
  
  write("### plot", filename, append = TRUE)
  write("ggplot(data, aes(x=x, y=y))", filename, append = TRUE)
  write("", filename, append = TRUE)
  
  write("### write output", filename, append = TRUE)
  write("write.table(data, \"data.csv\", row.names=FALSE, col.names=TRUE)", filename, append = TRUE)
  
  close(filename)
}