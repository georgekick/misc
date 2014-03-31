#' function: ptsKml
#' @param kml.name a character vector of a kml file name
#' @param icon.url a character vector of an icon URL
#' @param out.dir a character vector of an output file directory.The default is current directory
#' @param spdf spatial point data frame
#' @param type an optional argument of a column name indicating incident types
#' @return a kml point file
#' @export
#' @examples
#' \donttest{
#' 
#' icon.url <-  "http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png"
#' kml.name <- "observed.crime"
#' 
#' }

ptsKml<- function(kml.name, icon.url, out.dir, spdf, type){
  
  ## match.call return a call containing the specified arguments 
  ## and the function name also 
  ## I convert it to a list , from which I remove the first element(-1)
  ## which is the function name
  
  # check input arguments
  pars <- as.list(match.call()[-1])
  
  if (!is.na(pars$type){
    type.l <- length(table(spdf@data[,as.character(pars$type)]))
    temp <- as.data.frame(table(spdf@data[,as.character(pars$type)]))
    temp[,2] <- seq(1:type.l)
    names(temp) <- c(as.character(pars$type), "style")
    style <- merge(spdf@data, temp, all.x=TRUE)
    style <- as.vector(style[,"style"])
    rm(temp)
  } else {
    type.l <- 1
    style <- rep(1, nrow(spdf))
  }
  
  stopifnot(is.character(pars$kml.name) | is.na(pars$kml.name))
  stopifnot(is.character(pars$out.dir))
  stopifnot(is.character(pars$icon.url))
  stopifnot(class(pars$spdf)[1]=="SpatialPolygonsDataFrame")
  
  if (!is.na(pars$icon.url)){
    icon.url <- "https://dl.dropboxusercontent.com/u/121989515/kml/markers/icon57.png"
  } 
  
  stopifnot(length(icon.url)==length(type.l))
  
  # check coordinates
  stopifnot(!is.na(proj4string(pars$spdf))
  latlong <- "+init=epsg:4326"
  if (!proj4string(spdf)==CRS(latlong)){
    spdf <- spTransform(spdf, CRS(latlong))
  }
  
  #kmlPoints(obj=data.spdf2, kmlfile="test.kml", kmlname="point", kmldescription="",
  #    name=NULL, description="",
  #    icon=icon.url)
   
  if length(!is.na(out.dir)){
    filename <- file(paste(out.dir, "/", kml.name, ".kml", sep=""), "w",  blocking=FALSE)
  } else {
    filename <- file(paste(kml.name, ".kml", sep=""), "w",  blocking=FALSE)
  }
    
  write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", filename)
  write("<kml xmlns=\"http://earth.google.com/kml/2.2\">", filename, append = TRUE)
  write("<Document>", filename, append = TRUE)
  write(paste("<name>", kml.name, "</name>", sep=" "), filename, append = TRUE)
  write("<open>1</open>", filename, append = TRUE)
  
  for (i in 1:type.l){ 
    write(paste("<Style id=\"style", i, "\">", sep=""), filename, append = TRUE)
    write("<IconStyle>", filename, append = TRUE)
    write(paste("<Icon>
              <href>", icon.url[i], "</href>
              </Icon>", sep=""), filename, append = TRUE)
    write("</IconStyle>", filename, append = TRUE)
    write("</Style>", filename, append = TRUE)
  }
  
  for (i in 1:nrow(spdf)) {
    write("<Placemark>", filename, append = TRUE)
    #  write(paste("<name>", data.pred$Rdate[i], "</name>", sep=""), filename, append = TRUE)
    write(paste("<styleUrl>#style", style[i], "</styleUrl>", sep=""), filename, append=TRUE)
  
    write("<description>\n", filename, append = TRUE)
    #  for (j in 1:length(spdf[i,])){
    #    write(paste(names(spdf[j]), ": ", spdf[i,j], "\n ", sep=""), filename, append = TRUE)
    #  }        
    write(xtable(t(spdf@data[i,])), type="html", filename, append=TRUE)
    
    write("</description>", filename, append = TRUE)          
  
    write("<Point>", filename, append = TRUE)
    write("<coordinates>", filename, append = TRUE)
    write(paste(coordinates(spdf)[i,1], coordinates(spdf)[i,2], sep=","), filename, append = TRUE)
    write("</coordinates>", filename, append = TRUE)
    write("</Point>", filename, append = TRUE)
    write("</Placemark>", filename, append = TRUE)    				
  }
  write("</Document>", filename, append = TRUE)
  write("</kml>", filename, append = TRUE)
  close(filename)

}
