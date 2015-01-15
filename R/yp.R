#' function: yp
#' 
#' @param query a character string of search keyword
#' @param city a character string
#' @param state a character string of length two
#' @return data frame of yahoo local search results
#' @export 
#' @examples <Rcode>
#' \donttest{
#' query <- "drug store"
#' location <- "philadelphia, pa"
#' data <- yp(query="liquor store", city="Fresno", state="CA", appid=appid)
#' 
#' }" 
#' @example <path to R file>
#' @note <contents>
#' @section <name> : <contents>
#' @references <references>
# use the zipcode package 
# http://jeffreybreen.wordpress.com/2011/01/05/cran-zipcode/
# loop through zip code in a city and obtain places of interests
yp <- function(query, location){

  # check input arguments
  stopifnot(is.character(query))
  stopifnot(is.character(location))
  
  ## read zipcode data
  #library(zipcode)
  #data(zipcode)
  # subset zipcode data
  #call_quote <- substitute(subset(zipcode, city==a & state==b), list(a=city, b=state))
  #df <- eval(call_quote)
    
  library(XML)
  library(ggmap)  

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
  for(i in 1:ceiling(max.results/30)){
    
    Sys.sleep(runif(1, 7, 12))
        
    requestURL <- paste(URLprefix, query, "&geo_location_terms=", location, URLsuffix, i, sep="")
    temp <- suppressWarnings(try(readLines(requestURL)))
     
    # select a target line
    temp <- temp[grep(".&nbsp;", temp)]
      
    # separate a ingle line into rows
    temp <- unlist(strsplit(temp, ".&nbsp;"))
      
    # select lines with target information; remove unrelevant lines
    temp <- temp[grep("^[^<]", temp)]
    temp <- temp[grep("^[^\\\"]", temp)]
    temp <- temp[grep("^[^collection!]", temp)]
    
    temp <- temp[grep("addressLocality", temp)]
    
            
    web[[i]] <- temp
     
    cat("WebScraping: ", i, " / ", ceiling(max.results/30), "\n")
  }
  
  # process scpraped info
  yp.data <- data.frame(name=character(), address=character(), city=character(), state=character(), category=character(), lat=numeric(), long=numeric())
    
  for(i in 1:length(web)){
    
    cat("processing scraped data:", i, " / ", length(web), "\n")
        
    web2 <- web[[i]]
    
    name    <- gsub("(.*?)</a><.*", "\\1", web2)
    # replace a special character
    name    <- gsub("&amp; ", "& ", data$name)
    
    # phone number was difficult to get. ignored
    # phone   <- 
    
    address <- gsub(".*class=\\\"street-address\\\">(.*)</span><span itemprop=\\\"addressLocality\\\" .*", "\\1", web2)
    city    <- gsub(".*class=\\\"locality\\\">(.*), </span><span itemprop=\\\"addressRegion\\\".*", "\\1", web2)
    state   <- gsub(".*\"addressRegion\\\">(.*)<.*", "\\1", web2)
    
    # exclude information without street addresses
    
    name    <- name[grep("street-address", web2)]
    address <- address[grep("street-address", web2)]
    city    <- city[grep("street-address", web2)]
    state   <- state[grep("street-address", web2)]
    
    ## category was difficult to get. ignored (error prone)
    ## grab the first category 
    #category <- gsub(".*(<ul class=\\\"categories\\\">.*</ul>).*", "\\1", web2)
    #category <- gsub("(.*)</a></li>.*", "\\1", category)
    #category <- gsub(".*>(.*)</a></li><li>.*", "\\1", category)
    
    # geocode
    geo <- suppressMessages(geocode(paste(address, ", ", city, ", ", state, sep="")))
    
    # category is from query input, instead of yp output
    yp.data <- rbind(yp.data, data.frame(name=name, address=address, city=city, state=state, category=query, lat=geo$lat, long=geo$lon))
    
  } 
  yp.data
}
  
    
  