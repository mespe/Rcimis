## Key is stored separate and not shared

## Define function

##' Query the CIMIS API
##'
##' .. content for \details{} ..
##' @title Get CIMIS data
##' @param api_key a personal API key for accessing data
##' @param ... additional arguments in the form of "key = value" pairs
##'     passed along as query parameters
##' @param start start date, in ISO format ("YYYY-mm-dd")
##' @param end end date, in ISO format
##' @param url default URL to query
##' @return a data.frame object of query results
##'
##' @author Matthew Espe
##'
getCIMIS <- function(start, end,
                     api_key = getOption("Rcimis_key", stop("You need a key.")),
                     ..., .opts = list(),
                     format = "json",
                     url = "http://et.water.ca.gov/api/data",
                     parseJSON = TRUE)
{
  if(format != "json") {
      parseJSON = FALSE
      .opts[["httpheader"]] = c(Accept = "application/xml")
  }
      
  doc <- getForm(uri = url, 
                 startDate = start, endDate = end, ...,
                 appKey = api_key, .opts = .opts)
  if(!parseJSON)
     doc
  else
     fromJSON(doc, flatten = TRUE)$Data$Providers$Records[[1]]
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param api_key 
##' @param startyear 
##' @param endyear 
##' @param station_nbr 
##' @param include_qc 
##' @param ... 
##' @param .opts 
##' @return 
##' @author Matt Espe
##'
CIMISweather <- function(startyear, endyear, station_nbr,
                         include_qc = FALSE,
                         ...,
                         api_key = getOption("Rcimis_key", stop("You need a key.")),
                         .opts = list())
{
    ## Check "..." for valid options
    check_opts(args = ...)
    
    tmp <- getCIMIS(api_key = api_key,
                    start = paste0(startyear, '-01-01'),
                    end = paste0(endyear, '-12-31'),
                    unitOfMeasure = 'M',
                    targets = station_nbr, ...)

    if(!include_qc){
        idx <- grepl('[.]Qc$|[.]Unit$', colnames(tmp))
        tmp <- tmp[,-idx]
    }
 
    return(data)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param station_names 
##' @return 
##' @author Matt Espe
##'
get_station_info <- function(station_names){
  # Returns a comma separated list of station numbers
  # Given the station names

    stations <- getURL('http://et.water.ca.gov/api/station')
    tmp <- fromJSON(stations)
    i <- which(tmp$Stations$Name %in% station_names)
    data.frame(stn_nm = tmp$Stations$Name[i],
               stn_nbr = tmp$Stations$StationNbr[i],
               lat = do.call(rbind,strsplit(tmp$Stations$HmsLatitude[i], " / "))[,2],
               lon = do.call(rbind,strsplit(tmp$Stations$HmsLongitude[i], " / "))[,2],
               stn_ele = as.numeric(tmp$Stations$Elevation[i]) * 0.3048,
               stn_start = as.Date(tmp$Stations$ConnectDate[i], "%m/%d/%Y"),
               stn_end = as.Date(tmp$Stations$DisconnectDate[i], "%m/%d/%Y"),
               county = tmp$Stations$County[i],
               stringsAsFactors = FALSE)
  
}
