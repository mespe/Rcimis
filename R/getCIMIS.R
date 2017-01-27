## Key is stored separate and not shared

## Define function

##' Query the CIMIS API
##'
##' 
##' @title Get CIMIS data
##' @param api_key a personal API key for accessing data
##' @param ... additional arguments in the form of "key = value" pairs
##'     passed along as query parameters
##' @param start start date, in ISO format ("YYYY-mm-dd")
##' @param end end date, in ISO format
##' @param url default URL to query
##' @param parseJSON logical value indicating whether to process the result as JSON or just return it.
##'     If format is not "json, then this defaults to returning the document from CIMIS.
##' @return a data.frame object of query results
##'
##' @author Matthew Espe
##'
getCIMIS <- function(start, end,
                     api_key = getOption("Rcimis_key", stop("You need a key.")),
                     ...,
                     .opts = list(),
                     format = "json",
                     url = "http://et.water.ca.gov/api/data",
                     parseJSON = (format == "json"))
{
  if(format != "json") 
     .opts[["httpheader"]] = c(Accept = "application/xml")

   # Now combine all of the API parameters (except appKey) and validate
   # their names (and soon their values). We do this on the client side
   # to avoid/reduce errors on the server side.
  args = list(start = start, end = end, ...)
  args = check_opts(.args = args)
  args$appKey = api_key
  
  doc <- getForm(uri = url, 
                 .params = args,
                 .opts = .opts)
  if(!parseJSON)
     doc
  else
     fromJSON(doc, flatten = TRUE)$Data$Providers$Records[[1]]
}

##' 
##' This allows the R user to send a query to the CIMIS API to get various
##' data.
##' 
##' @title Get data from CIMIS API
##' @param api_key 
##' @param startyear 
##' @param endyear 
##' @param station_nbr 
##' @param include_qc 
##' @param ... 
##' @param .opts 
##' @return a data frame
##' @author Matt Espe
##'
CIMISweather <- function(startyear, endyear, station_nbr,
                         unitOfMeasure = 'M',
                         ...,
                         include_qc = FALSE,
                         api_key = getOption("Rcimis_key", stop("You need a key.")),
                         .opts = list())
{
    tmp <- getCIMIS(api_key = api_key,
                    start = paste0(startyear, '-01-01'),
                    end = paste0(endyear, '-12-31'),
                    unitOfMeasure = unitOfMeasure,
                    targets = station_nbr, ...)

    if(!include_qc){
        idx <- grepl('[.]Qc$|[.]Unit$', colnames(tmp))
        tmp <- tmp[,!idx]
    }
 
    return(data)
}

##' This queries details for all of the CIMIS weather stations.
##'
##' @title Get information about the CIMIS weather stations
##' @param station_names 
##' @return data.frame with elements describing the station name, number, latitude and longitude,
##'    elevation, start and end date of the station's activity, county, 
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
