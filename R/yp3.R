#' function: yp2
#' 
#' @param query a character string of search keyword
#' @param city a character string
#' @param state a character string of length two
#' @return data frame of yahoo local search results
#' @export 
#' @examples <Rcode>
#' \donttest{
#' wireless <- yp3(query="wireless store", location="Philadelphia, PA")
#' write.table(wireless, "Wireless.csv", col.names=TRUE, row.names=FALSE, sep=",")
#' 
#' }" 
#' @example <path to R file>
#' @note <contents>
#' @section <name> : <contents>
#' @references <references>
yp3 <- function(query, location){

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
  pb <- txtProgressBar(0.1, ceiling(max.results/30), style=3)
  for(i in 1:ceiling(max.results/30)){
    
    if (i==1){
      cat(paste("Estimated Completion Time:", Sys.time()+seconds(10*ceiling(max.results/30)+max.results), "\n"))   
      cat("WebScraping", ceiling(max.results/30), "pages ", "\n")
    }  
      Sys.sleep(runif(1, 7, 12))
    
    # create URL and access it
    requestURL <- paste(URLprefix, query, "&geo_location_terms=", location, URLsuffix, i, sep="")
    temp <- suppressWarnings(try(readLines(requestURL)))
     
    # select a target line
    temp <- temp[8] 
    temp <- gsub(".*expandedMapListings\":\\[(.*)\\],\"srpSortTerm.*", "\\1", temp) 
            
    # separate a ingle line into rows
    temp <- unlist(strsplit(temp, "\"}"))
      
    web[[i]] <- temp
     
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # process scpraped info
  yp.data <- data.frame(id=character(), name=character(), lat=numeric(), lon=numeric(),  address=character(), city=character(), state=character(), zip=character())
  
  pb <- txtProgressBar(0.1, length(web), style=3)
    
  for(i in 1:length(web)){
    
    if (i==1){
      cat("Processing a total of", length(web), "scraped pages (appx.", length(web)*30, "records)", "\n")
    }
        
    web2 <- web[[i]]
    
    id      <- gsub(".*listingID\\\":\\\"([0-9]*)\\\",.*", "\\1", web2)
    name    <- gsub(".*name\\\":\\\"(.*)\\\",\\\"distance.*", "\\1", web2)
    lat     <- as.numeric(gsub(".*latitude\\\":(-{0,1}[0-9]*\\.[0-9]*),.*", "\\1", web2))
    lon     <- as.numeric(gsub(".*longitude\\\":(-{0,1}[0-9]*\\.[0-9]*),.*", "\\1", web2))
    address <- gsub(".*addressLine1\\\":\\\"(.*)\\\",\\\"city.*", "\\1", web2)
    city    <- gsub(".*city\\\":\\\"(.*)\\\",\\\"state.*", "\\1", web2)
    state   <- gsub(".*state\\\":\\\"(.*)\\\",\\\"zip.*", "\\1", web2)
    zip     <- gsub(".*zip\\\":\\\"(.*)\\\",\\\"omitAddress.*", "\\1", web2)
    
    # exclude information without street addresses
    
    #id      <- id[grep("street-address", web2)]
    #name    <- name[grep("street-address", web2)]
    #address <- address[grep("street-address", web2)]
    #city    <- city[grep("street-address", web2)]
    #state   <- state[grep("street-address", web2)]
    #phone   <- phone[grep("street-address", web2)]
    
    # category is from query input, instead of yp output
    yp.data <- rbind(yp.data, data.frame(id=id, name=name, address=address, city=city, state=state, lat=lat, lon=lon))
    
    setTxtProgressBar(pb, i)
  } 
  close(pb)
  beep()
  
  yp.data
}
  
    
  