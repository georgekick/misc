#' function: yp2
#' 
#' @param query a character string of search keyword
#' @param city a character string
#' @param state a character string of length two
#' @return data frame of yahoo local search results
#' @export 
#' @examples <Rcode>
#' \donttest{
#' liquor <- yp2(query="liquor store", location="Philadelphia, PA")
#' drug   <- yp2(query="drug store", location="Philadelphia, PA")
#' check  <- yp2(query="check cashing service", location="Philadelphia, PA")
#' gas  <- yp2(query="Gas Station", location="Philadelphia, PA")
#' donut  <- yp2(query="Donut Shops", location="Philadelphia, PA")

#' pawnshops  <- yp2(query="Pawnbrokers", location="Philadelphia, PA")
#' write.table(pawnshops, "PawnshopsPhil.csv", col.names=TRUE, row.names=FALSE, sep=",")
#' pawnshops  <- yp2(query="Pawnbrokers", location="Fresno, CA")
#' write.table(pawnshops, "PawnshopsCA.csv", col.names=TRUE, row.names=FALSE, sep=",")
#' 
#' }" 
#' @example <path to R file>
#' @note <contents>
#' @section <name> : <contents>
#' @references <references>
yp2 <- function(query, location){

  # using rvest may result in a more succinct function
  # use selectorgadget along with rvest
  # e.g.,
  # name <- html_text(html_nodes(html, "#main-content .business-name"))
  
  # check input arguments
  stopifnot(is.character(query))
  stopifnot(is.character(location))
     
  library(XML)
  library(ggmap)  
  library(beepr)
  library(lubridate)

  # sample URL  "http://www.yellowpages.com/search?search_terms=drug%20store&geo_location_terms=philadelphia%2C%20pa&page=1"
  
  URLprefix <- "http://www.yellowpages.com/search?search_terms="
  URLsuffix <- "&page="
  
  # 5 is used because the max count for page 1 can sometimes be wrong 
  requestURL <- paste(URLprefix, query, "&geo_location_terms=", location, URLsuffix, "5", sep="")
  web <- suppressWarnings(try(readLines(requestURL)))
  max.results <- as.numeric(gsub(".* ([0-9]{1,})<span>results</span>.*", "\\1", 
                                 web[grep("<span>results</span>", web)]))

  # scrape web
  web <- list()
  pb <- txtProgressBar(0, ceiling(max.results/30), style=3)
  for(i in 1:ceiling(max.results/30)){
    
    if (i==1){
      cat(paste("\nEstimated Completion Time:", Sys.time()+seconds(10*ceiling(max.results/30)+max.results), "\n"))   
      cat("WebScraping", ceiling(max.results/30), "pages ", "\n")
    }  
      Sys.sleep(runif(1, 7, 12))
    
    # create URL and access it
    requestURL <- paste(URLprefix, query, "&geo_location_terms=", location, URLsuffix, i, sep="")
    temp <- suppressWarnings(try(readLines(requestURL)))
     
    # select a target line
    temp <- grep("<h3 class=\"n\">", temp, value=TRUE)
            
    # separate a ingle line into rows
    temp <- unlist(strsplit(temp, "<div class=\"info\">"))
      
    # select lines with target information; remove unrelevant lines
    temp <- grep("<h3 class=\"n\">[0-9]{1,}. <.*", temp, value=TRUE)
                   
    web[[i]] <- temp
     
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # process scpraped info
  yp.data <- data.frame(id=character(), name=character(), address=character(), city=character(), state=character(), category=character(), phone=character(), lat=numeric(), long=numeric())
  
  pb <- txtProgressBar(0, length(web), style=3)
    
  for(i in 1:length(web)){
    
    if (i==1){
      cat("\nProcessing a total of", length(web), "scraped pages (appx.", length(web)*30, "records)", "\n")
    }
        
    web2 <- web[[i]]
    
    id      <- gsub("<h3 class=\"n\">([0-9]{1,}). <.*", "\\1", web2)
    
    name    <- gsub(".*class=\"business-name\">(.*?)</a>.*", "\\1", web2)
    # replace a special character
    name    <- gsub("&amp; ", "& ", name)
    
    address <- gsub(".*class=\"street-address\">(.*?)<.*", "\\1", web2)
    city    <- gsub(".*class=\"locality\">(.*?), <.*", "\\1", web2)
    state   <- gsub(".*\"addressRegion\">(.*?)<.*", "\\1", web2)
    
    phone   <- gsub(".*\"phones phone primary\">(.*?)<.*", "\\1", web2)
    
    # exclude information without street addresses
    
    id      <- id[grep("street-address", web2)]
    name    <- name[grep("street-address", web2)]
    address <- address[grep("street-address", web2)]
    city    <- city[grep("street-address", web2)]
    state   <- state[grep("street-address", web2)]
    phone   <- phone[grep("street-address", web2)]
    
    ## category was difficult to get. ignored (error prone)
    ## grab the first category 
    #category <- gsub(".*(<ul class=\\\"categories\\\">.*</ul>).*", "\\1", web2)
    #category <- gsub("(.*)</a></li>.*", "\\1", category)
    #category <- gsub(".*>(.*)</a></li><li>.*", "\\1", category)
    
    # geocode
    geo <- suppressMessages(geocode(paste(address, ", ", city, ", ", state, sep="")))
    
    # category is from query input, instead of yp output
    yp.data <- rbind(yp.data, data.frame(id=id, name=name, address=address, city=city, state=state, category=query, phone=phone, lat=geo$lat, long=geo$lon))
    
    setTxtProgressBar(pb, i)
  } 
  close(pb)
  beep()
  
  yp.data
}
  
    
  