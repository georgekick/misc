#' function: gg_dens_animate
#' @param data data frame with date, lon, and lat columns
#' @param date (required) a column name of class POSIXct
#' @param interval (required) a numeric vector indicating density animation intervals (days), commonly 7,  30, or 365
#' @param h (required) a numeric vector of std. dev. of density
#' @param n (required) cell count for density estimation in xy directions (a vector of length one or two)
#' @param ggmap (optional) a ggmap object used as a background map 
#' @param output (required) a character vector of length 1 (HTML or Movie)
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
#' data <- data[data$CAS=="BURG",]
#' fresno <- get_map(location=c(left = -119.93271, bottom =  36.67063, right = -119.64477, top = 36.92134), maptype = 'roadmap')
#' fresno.map <- ggmap(fresno)
#' 
#' ani.options(ani.width=720, ani.height=720, htmlfile="gg_dens.html", verbose=FALSE)
#' # ani.options(outdir=getwd())
#' 
#' gg_dens_animate(data=data, date="Rdate", ggmap=fresno.map, interval=30)
#' 
#' }
gg_dens_animate <- function(data, date, ggmap, output="HTML", 
                            interval=30, h=0.02, n=200, limits=c(0, 250),
                            col.low="white", col.high="red"){
  
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
  data <- data[order(data[,pars$date]),]
  data$id <- difftime(data[,pars$date], data[1,pars$date], units="days")
    
  data$id <- as.numeric(floor(data$id / interval))
  
  index.table <- as.data.frame(table(data$id))
  names(index.table)[1] <- c("index")
  index.table$index <- as.numeric(as.character(index.table$index))
  temp <- data.frame(index=seq(0:max(index.table$index)))
  index.table <- merge(temp, index.table, all.x=TRUE, all.y=TRUE)  
  index.table$Freq[is.na(index.table$Freq)] <- 0
  index.table <- index.table[index.table$Freq>0, ]

  min.y <- year(min(data[,pars$date]))
  min.m  <- month(min(data[,pars$date]))
  min.d  <- day(min(data[,pars$date]))
  
  plotIncident <- function(.id){
    
  df <- subset(data, id == .id)
  
  begin.y <- year(min(data[,pars$date] + days(interval * .id)))
  begin.m  <- month(min(data[,pars$date] + days(interval * .id)))
  begin.d  <- day(min(data[,pars$date] + days(interval * .id)))
  
  end.y <- year(min(data[,pars$date] + days(interval * (.id + 1))))
  end.m  <- month(min(data[,pars$date] + days(interval * (.id + 1))))
  end.d  <- day(min(data[,pars$date] + days(interval * (.id + 1))))
  
  begin <- paste(begin.y, begin.m, begin.d, sep="/")
  end <- paste(end.y, end.m, end.d, sep="/")
  
  p1 <- ggmap + 
    # stat_density2d(aes(x = lon, y = lat, fill = ..level..,
    #    alpha = ..level..), h=0.025, n=100, size = 2, bins = 4, data = df, geom = "polygon") + 
    stat_density2d(aes(x = lon, y = lat, fill = ..level..,
        alpha = ..level..), h=h, n=n, data = df, 
        lims=c(range(ggmap$data$lon), range(ggmap$data$lon)),
        geom = "polygon") + 
    geom_point(aes(x = lon, y=lat), data=df) + 
    ggtitle(paste(begin, "-", end, "\n", 
                  index.table$Freq[index.table$index==.id], " incidents", 
                    sep="")) + 
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank()) +
    # guides(fill=FALSE, alpha=FALSE)
    scale_fill_gradient(limits=limits, low=col.low, high=col.high) + 
    guides(alpha=FALSE)
  }
  
  if (output =="Movie"){
    saveMovie(
      for (i in 0:max(index.table$index)){
        print(plotIncident(i))
        print(paste(round((i+1)/nrow(index.table) * 100), "% complete", sep=""))
        ani.pause()
      }, 
      clean = T)
  } else if (output=="HTML"){
    saveHTML(
      for (i in 0:max(index.table$index)){
        print(plotIncident(i))
        print(paste(round((i+1)/nrow(index.table) * 100), "% complete", sep=""))
        ani.pause()
      }
      , clean = T)
  }
}
