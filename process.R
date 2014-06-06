library(RCurl)
library(RJSONIO)


construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}


gGeoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- construct.geocode.url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    return(c(lat, lng))
  } else {
    return(c(NA,NA))
  }
}


#' Geotag dataframe via Google geo tag service
#'
#' @param df : dataframe to geocode
#' @param geovars : character vector with variable names that you want geotagged
#' @param geodata : you can pass longitude and latitude as a dataframe to fill up where there are NA's
#' @return dataframe with longitude and latitude values of nrow() equal to dataframe that was passed
#' @export
#' @examples
#' geotagDataframe(data, c("Street", "City", "Country"))
geotagDataframe <- function(df, geovars, main=NULL, previous_geodata=NULL) 
{
  
  longitude <- vector() #dlugos geo
  latitude <- vector() #szerokosc geo
  if( is.null( previous_geodata) == FALSE ){
    longitude <- previous_geodata$longitude
    latitude <- previous_geodata$latitude
  }
  if (NROW(df) == 0L) 
    stop("dataframe has no rows")
  if (NCOL(df) == 0L) {
    stop("dataframe has no columns")
  }
  if (!is.vector(geovars)) 
    stop("'by' must be a vector")
  geovar <- do.call("paste",c(df[geovars], sep = ", "))
  mainvar <- do.call("paste",c(df[main], sep = ", "))
  for(i in 1:nrow(df)){
    if(is.na(longitude[i])){
      coor <- gGeoCode(paste(geovar[i]) )
      if(!is.na(coor[1])){
      latitude[i] <- coor[1]
      longitude[i] <- coor[2]
      }
      else{
        if(!is.null(main)){
          coor <- gGeoCode(paste(mainvar[i]) )
          latitude[i] <- coor[1]
        longitude[i] <- coor[2]
        }
      
      }
    }  
  }
  geodata <- data.frame(longitude,latitude)
  geodata
}




setwd("~/Diplomatic trips/")

trips <- read.csv("presidents_trips_1945_2014.csv", encoding="utf8")
trips <- trips[,2:ncol(trips)]
colnames(trips) <- c("start", "end", "country", "cities", "reason", "president")
trips$year <- gsub(pattern="\\d{1,2}-[A-Za-z]{3}-",replacement="",x=trips$start) # assume no visits at the turn of the year

#clean whitespaces and some weird characters
atrips <- data.frame(apply(trips, MARGIN=2, function(x) gsub("^Â\\s+|\\s+$", "",x))) #  strip whitespaces)
atrips <- data.frame(apply(atrips, MARGIN=2, function(x) gsub("^\\s+|\\s+$", "",x))) #  strip whitespaces)

#validate country names and extract country code
library(countrycode)

atrips$country_code <- countrycode(atrips$country ,origin="country.name", destination="iso3c")

#verify country code vector. 6 NAs, Kosovo, Czechoslovakia, Yugoslavia. The reasonable solution would be to ascribe 
# czech republic to czechoslovakia and smth to yogoslavia, but really we need historical maps. So just forget about that
# for a moment, it;s only 6 cases
table(atrips$country_code,useNA="always")
subset(atrips, is.na(atrips$country_code) ) 


geodata <- geotagDataframe( atrips, c("cities", "country"))

geodata <- geotagDataframe( atrips, c("cities", "country"),previous_geodata = geodata)

geodata <- geotagDataframe(atrips, c("cities", "country"),c("cities"), previous_geodata=geodata)

atrips <- cbind(geodata, atrips)


### eliminate NA

geodata_nona <- subset(geodata, !is.na(atrips$latitude))

white_house <- gGeoCode("The White House, Pennsylvania Avenue Northwest, Washington, USA")


datamaps_make_trip<- function (origin, destination){
return (paste0(" {
      origin: {
          latitude: ",origin[1],",
          longitude: ",origin[2],"
      },
      destination: {
          latitude: ",destination[1],",
          longitude: ",destination[2],"
      },
      options: {
        strokeColor: 'rgba(100, 10, 200, 0.3)'
      }
  }"))
}



datamaps_make_arcs <- function(origin, destination_df, strokeWidth=1, arcSharpness=1.4){
  arcs = list()
  for(i in 1:nrow(destination_df)){
    arcs[i] <- datamaps_make_trip(origin, c(destination_df$latitude[i],destination_df$longitude[i]) )
  }
  return ( paste0( "arcs.arc([", paste(arcs, collapse=", "), "],  {strokeWidth:",strokeWidth,", arcSharpness: ",
                   arcSharpness ,"});" ))
}


html_stuff = '<script src="http://d3js.org/d3.v3.min.js"></script>
<script src="http://d3js.org/topojson.v1.min.js"></script>
<script src="datamaps/dist/datamaps.world.min.js"></script>
<div id="arcs" style="position: relative; width: 1500px; height: 800px;"></div>
<script>'
script_closing <- "</script>"

arcs_obj = "
var arcs = new Datamap({
  element: document.getElementById(\"arcs\"),
  scope: 'world',
  fills: {
    defaultFill: \"#ABDDA4\",
    origin: '#0fa0fa'
  },
  data: {
    'USA': { fillKey: 'origin' },
  }
});
"


arcs_data <- datamaps_make_arcs(white_house, subset(geodata_nona, select=c("latitude","longitude")))



cat(paste0(html_stuff, arcs_obj, arcs_data, script_closing), file="diplomatic_trips.html")












