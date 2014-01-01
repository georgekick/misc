#' function: gg_pts_animate
#' @param data data frame with date, lon, and lat columns
#' @param date (required) a column name of class POSIXct
#' @param ggmap (optional) a ggmap object used as a background map 
#' @param output (required) a character vector of length 1 (HTML or Movie)
#' @param transp a logical vector indicating if old incidents are shown transparently
#' @return an HTML file
#' @export
#' @examples
#' \donttest{
#' library(ggplot2); library(lubridate)
#' library(maps); library(animation)
#' library(rgdal); library(ggmap)
#' library(grid); library(scales)
#' data <- read.csv("D:\\Users\\gkikuchi\\Google Drive\\FresnoDailyCrimeRollingData\\FresnoCrimeDailyRoll.csv", stringsAsFactors = F)
#' data$Rdate <- ymd_hms(data$Rdate)
#' data <- data[data$CAS=="HOMI",]
#' fresno <- get_map(location=c(left = -119.93271, bottom =  36.67063, right = -119.64477, top = 36.92134), maptype = 'roadmap')
#' fresno.map <- ggmap(fresno)
#' 
#' ani.options(ani.width=720, ani.height=720, htmlfile="gg_pts.html", verbose=FALSE)
#' # ani.options(outdir=getwd())
#' gg_pts_animate(data=data, date="Rdate", ggmap=fresno.map)
#' 
#' }
gg_pts_animate <- function(data, date, ggmap, output="HTML", transp=TRUE){

  # browser()
  # check input arguments ----
  pars <- as.list(match.call()[-1])
  
  # required arguments
  stopifnot(class(data)[1]=="data.frame")
  stopifnot(class(data[,pars$date])[1]=="POSIXct" | class(data[,pars$date])[1]=="POSIXt")
  
  data[,pars$date] <- as.Date(data[,pars$date]) # change to Date class in order to use scale_x_date
  data <- data[order(data[,pars$date]),]

  # optional arguments
  if (!missing(ggmap)){
    stopifnot(class(ggmap)[1]=="gg" | class(ggmap)[1]=="ggplot")
  }
  stopifnot(output=="HTML" | output=="Movie")
  
  # create id for each date of occurrence and merge with the original data
  id.table <- as.data.frame(table(data[,pars$date]))
  names(id.table) <- c(pars$date, "id")  
  id.table[,pars$date] <- as.Date(as.character(id.table[,pars$date]))
  id.table$id <- seq(1:nrow(id.table))
  data <- merge(data, id.table)
 
plotIncident <- function(.id){

  df <- subset(data, id <= .id)
  yr <- paste(year(df[df$id==.id, pars$date])[1], 
              month(df[df$id==.id, pars$date], label=TRUE, abbr=FALSE)[1], 
              day(df[df$id==.id, pars$date])[1], sep=" ")
  
  min.y <- year(min(data[,pars$date]))
  min.m  <- month(min(data[,pars$date]))
  min.d  <- day(min(data[,pars$date]))
  
  if (transp == TRUE){
    df$transp2 <- df$id / .id
  } else {
    df$transp2 <- 1
  }
  
  p1 <- ggmap + 
    # new incident
    geom_point(data=df[df$id==.id,], aes(x=lon, y=lat), colour = alpha("red", 0.7), size = 8) +
    # old incidents
    geom_point(data=df, aes(x=lon, y=lat), color = alpha("yellow", df$transp2), size = 4) +
            
    ggtitle(paste(yr, "\n", nrow(df), " incidents since ", min.y, "-", min.m, "-", min.d, 
                    sep="")) +  
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank())
}

if (output =="Movie"){
  saveMovie(
    for (i in 1:nrow(id.table)){
      print(plotIncident(i))
      print(paste(round(i/nrow(id.table) * 100), "% complete", sep=""))
      ani.pause()
    }, 
    clean = T)
  } else if (output=="HTML"){
  saveHTML(
    for (i in 1:nrow(id.table)){
      print(plotIncident(i))
      print(paste(round(i/nrow(id.table) * 100), "% complete", sep=""))
      ani.pause()
    }
    , clean = T)
  }
}
