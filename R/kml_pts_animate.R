#' function: kml_pts_animate
#' @param kml.name (required) a character vector of a kml file name
#' @param spdf (required) spatial polygon data frame
#' @param date (required) date column of type POSIXct
#' @param type (optional) a column name indicating incident types
#' @param icon.url (optional) a character vector of an icon URL
#' @param col (optional) icon color from the following: c("blue", "grn", "ltblu", "pink", "purple", "red", "wht", "ylw")
#' @param out.dir (optional) a character vector of an output file directory.The default is current directory
#' @param name.col (optional) a column name for a point feature name (default is no name/id)
#' @param name.seq (optional) logical vector of whether to use an sequence  optional argument of a column name for a point feature name (default is no name/id)
#' @return a kml point file
#' @export
#' @examples
#' \donttest{
#' library(lubridate)
#' library(sp)
#' library(xtable)
#' icon.url <-  "http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png"
#' kml.name <- "observed.crime"
#' data <- read.csv("D:\\Users\\gkikuchi\\Google Drive\\FresnoDailyCrimeRollingData\\FresnoCrimeDailyRoll.csv", stringsAsFactors = F)
#' data$Rdate <- ymd_hms(data$Rdate)
#' data <- subset(data, CAS=="HOMI" | CAS=="NARC" | CAS=="FRAD" | CAS=="WEAP")
#' data <- data[,!names(data) %in% "UCR_DESCRIPTION"]
#' spdf <- SpatialPointsDataFrame(data=data, coords=matrix(c(data$lon, data$lat), ncol=2), 
#'                  proj4string=CRS("+init=epsg:4326")) # latlong
#' 
#' # error (type != col)
#' kml_pts_animate(kml.name="temp7", spdf=spdf, date="Rdate", type="CAS", col=c("white", "yellow"), name.col="CASE_NO")
#' 
#' kml_pts_animate(kml.name="temp8", spdf=spdf, date="Rdate", out.dir="temp", type="CAS", name.col="CASE_NO")
#' kml_pts_animate(kml.name="temp9", spdf=spdf, date="Rdate", out.dir="temp", type="CAS", name.seq=TRUE)
#' }
kml_pts_animate <- function(kml.name, spdf, date, type, icon.url, out.dir, col, name.col, name.seq=FALSE){
  # browser()
  # check input arguments ----
  pars <- as.list(match.call()[-1]) # remove the first element(-1) which is the function name
  
  # required arguments
  stopifnot(is.character(kml.name) | missing(kml.name))
  stopifnot(class(spdf)[1]=="SpatialPointsDataFrame")
  stopifnot(class(spdf@data[,pars$date])[1]=="POSIXct" | class(spdf@data[,pars$date])[1]=="POSIXt")
  
  # optional arguments
  if (!missing(type)){
    type.l <- length(table(spdf@data[,as.character(pars$type)]))
    temp <- as.data.frame(table(spdf@data[,as.character(pars$type)]))
    temp[,2] <- seq(1:type.l)
    names(temp) <- c(as.character(pars$type), "style")
    spdf@data <- merge(spdf@data, temp, all.x=TRUE)
    # style <- as.vector(style[,"style"])
  } else {
    type.l <- 1
    spdf@data$style <- rep(1, nrow(spdf))
  }
  
  if (!missing(col)){
    if(!type.l==length(col)) stop("the number of incident types and colors do not match")
  }
  if (!missing(out.dir)){
    stopifnot(is.character(out.dir))
  }
  if (!missing(icon.url)){
    stopifnot(is.character(icon.url))
  } 
  
  col.temp <- c("blue", "grn", "ltblu", "pink", "purple", "red", "wht", "ylw")
  if (!missing(col)){
    col <- gsub("green", "grn", col)
    col <- gsub("light blue", "ltblu", col)
    col <- gsub("white", "wht", col)
    col <- gsub("yellow", "ylw", col)
    col.temp <- col.temp[col.temp %in% col]
  }
  
  if (missing(icon.url) & type.l==1){
    icon.url <- "https://dl.dropboxusercontent.com/u/121989515/kml/markers/icon57.png"
  } else if (missing(icon.url) & type.l>1) {
    stopifnot(!type.l>8)
    col <- col.temp[1:type.l]
    for (i in 1:length(col)){
      icon.url <- lapply(col, function(x) paste0("http://maps.google.com/mapfiles/kml/pushpin/", x, "-pushpin.png"))
    }   
  }
  
  stopifnot(length(icon.url)==type.l)
  
  # check coordinates
  stopifnot(!is.na(proj4string(spdf)))
  latlong <- "+init=epsg:4326"
  if (!proj4string(spdf)==CRS(latlong)@projargs){
    spdf <- spTransform(spdf, CRS(latlong))
  }
  
  #kmlPoints(obj=data.spdf2, kmlfile="test.kml", kmlname="point", kmldescription="",
  #    name=NULL, description="",
  #    icon=icon.url)
  
  # generate date/time variables
  Year <- year(spdf@data[,pars$date])
  Month <- month(spdf@data[,pars$date])
  for (i in 1:length(Month)){
    if (nchar(as.character(Month[i]))==1){
      Month[i] <- paste("0", Month[i], sep="")
    }
  }
  
  Day <- day(spdf@data[,pars$date])
  for (i in 1:length(Day)){
    if (nchar(as.character(Day[i]))==1){
      Day[i] <- paste("0", Day[i], sep="")
    }
  }
  
  # write a KML file -----
  if (!missing(out.dir)){
    filename <- file(paste(out.dir, "/", kml.name, ".kml", sep=""), "w",  blocking=FALSE)
  } else {
    filename <- file(paste(kml.name, ".kml", sep=""), "w",  blocking=FALSE)
  }
  
  write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", filename)
  write("<kml xmlns=\"http://earth.google.com/kml/2.2\">", filename, append = TRUE)
  write("<Document>", filename, append = TRUE)
  write(paste("<name>", kml.name, "</name>", sep=" "), filename, append = TRUE)
  write("<open>0</open>", filename, append = TRUE)
  
  for (i in 1:type.l){ 
    write(paste("<Style id=\"style", i, "\">", sep=""), filename, append = TRUE)
    write("<IconStyle>", filename, append = TRUE)
    write(paste("<Icon><href>", icon.url[i], "</href></Icon>", sep=""), filename, append = TRUE)
    write("</IconStyle>", filename, append = TRUE)
    write("</Style>", filename, append = TRUE)
  }
  
  for (i in 1:nrow(spdf)) {
    write("<Placemark>", filename, append = TRUE)
    
    if (!missing(name.col)){
      stopifnot(length(data[, pars$name.col])>0)
      write(paste("<name>", data[i, pars$name.col], "</name>", sep=""), filename, append = TRUE)
    } 
    if (missing(name.col) & (name.seq==TRUE)){
      write(paste("<name>", i, "</name>", sep=""), filename, append = TRUE)
    }
    
    write(paste("<styleUrl>#style", spdf@data$style[i], "</styleUrl>", sep=""), filename, append=TRUE)
    
    write("<description>\n", filename, append = TRUE)
    write(print(xtable(t(spdf@data[i,])), 
                type="html", 
                include.colnames=FALSE,
                html.table.attributes = "border=\"1\"")
          , filename, append=TRUE)
    
    write("</description>", filename, append = TRUE)          
    
    write("<TimeStamp>", filename, append = TRUE)
    write(paste("<when>", Year[i], "-", Month[i], "-", Day[i], "</when>", sep=""), filename, append = TRUE)
    write("</TimeStamp>", filename, append = TRUE)
    
    
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
