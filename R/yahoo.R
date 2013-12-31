#' function: yahoo
#' 
#' yahoo local does not work anymore; try yellowpage or other sites
#' 
#' @param query a character string of search keyword
#' @param city a character string
#' @param state a character string of length two
#' @param <name> <description>
#' @param <name> <description>
#' @return data frame of yahoo local search results
#' @export 
#' @examples <Rcode>
#' \donttest{
#' appid<-'YD-9G7bey8_JXxQP6rxl.fBFGgCdNjoDMACQA--'
#' yahoo(query="liquor store", city="Fresno", state="CA", appid=appid)
#' 
#' }" 
#' @example <path to R file>
#' @note <contents>
#' @section <name> : <contents>
#' @references <references>
# use the zipcode package 
# http://jeffreybreen.wordpress.com/2011/01/05/cran-zipcode/
# loop through zip code in a city and obtain places of interests
yahoo <- function(query, city, state, appid){

  # check input arguments
  stopifnot(is.character(query))
  stopifnot(is.character(city))
  stopifnot(is.character(state))
  
  # read zipcode data
  library(zipcode)
  data(zipcode)
  
  # subset zipcode data
  call_quote <- substitute(subset(zipcode, city==a & state==b), list(a=city, b=state))
  df <- eval(call_quote)
    
  library(XML)
    
  ########### tweak to obtain totalResultsAvailable
  
  max.results <- 165 # page count; max=250
  
  # sample URL  "http://local.yahooapis.com/LocalSearchService/V3/localSearch?appid=YahooDemo&query=pizza&zip=94306&results=2"
  
  URLprefix <- "http://local.yahooapis.com/LocalSearchService/V3/localSearch?appid="
  URLsuffix <- "&results=1"
  
  myYahooTable <- data.frame(name=character(), phone=character(), address=character(), city=character(), state=character(), lat=numeric(), long=numeric(), EID=numeric())
  
  for(i in 1:max.results){
    requestURL <- paste(URLprefix, appid, "&query=", query, "&start=", i, "&city=", city, "&state=", state, URLsuffix, sep="")
    
    cat("WebScraping result=", i, "\n")
    
    tryCatch({
      xmlResult <- xmlTreeParse(requestURL, isURL=TRUE, addAttributeNamespaces=TRUE)
      
      geoResult <- xmlResult$doc$children$ResultSet[[2]]
      
      if (length(xmlValue(geoResult[['Title']]))>0) Name <- xmlValue(geoResult[['Title']])
      else Name <- NA
      if (length(xmlValue(geoResult[['Phone']]))>0) Phone2 <- xmlValue(geoResult[['Phone']])
      else Phone2 <- NA
      if (length(xmlValue(geoResult[['Address']]))>0) Address <- xmlValue(geoResult[['Address']])
      else Address <- NA
      if (length(xmlValue(geoResult[['City']]))>0) City2 <- xmlValue(geoResult[['City']])
      else City2 <- NA
      if (length(xmlValue(geoResult[['State']]))>0) State2 <- xmlValue(geoResult[['State']])
      else State2 <- NA
      if (length(xmlValue(geoResult[['Latitude']]))>0) Lat <- as.numeric(xmlValue(geoResult[['Latitude']]))
      else Lat <- NA
      if (length(xmlValue(geoResult[['Longitude']]))>0) Long <- as.numeric(xmlValue(geoResult[['Longitude']]))
      else Long <- NA
      myYahooTable <- rbind(myYahooTable, data.frame(name = Name, phone = Phone2, address = Address, city = City2, state = State2, lat = Lat, long = Long, EID = NA))
    })
    Sys.sleep(1) #this pause helps keep Yahoo happy
  }
  
  myYahooTable$ID <- as.numeric(rownames(myYahooTable))
  
  myYahooTable
  


}
