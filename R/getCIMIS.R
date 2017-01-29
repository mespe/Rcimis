## Key is stored separate and not shared

## Define function

##' Query the CIMIS API
##'
##' 
##' @title Get CIMIS data
##' @param start start date, in ISO format ("YYYY-mm-dd")
##' @param end end date, in ISO format
##' @param targets Either the station number, the zip code, address, or lat/long separated by commas. See API documentation for more information 
##' @param unitOfMeasure "M" for metric units, "E" for empirial, defaults to "E"
##' @param dataItems optional, if not provided will return all daily variables 
##' @param prioritizeSCS should data from Spatial CIMIS (interpolated) be prioritized?
##' @param appKey The API key for CIMIS. By default, the function looks in getOption
##' @param .opts Additional options passed to \code{getForm}
##' @param format XML or JSON, defaults to JSON
##' @param url default URL to query
##' @param parseJSON logical value indicating whether to process the result as JSON or just return it.
##'     If format is not "json, then this defaults to returning the document from CIMIS.
##'
##' @return a data.frame, XML, or JSON (depending on above options) object of query results
##'
##' @author Matthew Espe and Duncan Temple Lang
##'
##' @examples
##' ## Get Evapo-transpiration for Jan 2015 in Metric units from station 6
##' getCIMIS("2015-01-01", "2015-02-01", 6, "M", "day-eto", FALSE)
##'
##' 
getCIMIS = function(start, end,
                     targets = NA,
                     unitOfMeasure = NA,
                     dataItems = NA,
                     prioritizeSCS = NA,
                     appKey = getOption("Rcimis_key", stop("You need a key.")),                     
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

  args = list(startDate = start, endDate = end, unitOfMeasure = unitOfMeasure,
              dataItems = matchDataItems(dataItems),
              prioritizeSCS = prioritizeSCS, targets = targets)

  args = checkParams(.args = args)
  args$appKey = appKey

  if("dataItems" %in% names(args))
     args$dataItems = paste(matchDataItems(args$dataItems), collapse = ",")
  
  
  doc = getForm(uri = url, 
                 .params = args,
                 .opts = .opts)
  if(!parseJSON)
     doc
  else
     fromJSON(doc, flatten = TRUE)$Data$Providers$Records[[1]]
}

##' 
##' This allows the R user to send a general query to the CIMIS API to get various
##' data from the API.
##' 
##' This is a general function to get data from the CIMIS API that takes arbitrary API parameters.
##' 
##' @title Get data from CIMIS API
##' @param startDate The start date, in ISO format (e.g. "YYYY-MM-DD"). Cannot be earlier than 1987-06-7 
##' @param endDate The end date in ISO format
##' @param targets The CIMIS assigned station number, zip code, address, or Lat/Long (comma separated). See \code{StnInfo} dataset for list of stations.
##' @param unitOfMeasure "M" for metric units, "E" for empirical
##' @param dataItems optional. If not provided, function will return all variables
##' @param prioritizeSCS Should results from Spatial CIMIS (interpolated) be prioritized?
##' @param ... Additional arguments passed to the CIMIS API. 
##' @param includeQC Should columns with quality control flags be included in results?
##' @param appKey The API key. By default, the function checks \code{getOptions} for the api as "Rcimis_key".  This avoids exposing this private, secure information in scripts and console.
##' @param .opts options passed to \code{getForm} to control the RCurl HTTP request.
##'
##' @return a data frame, JSON object, or XML object, depending on options passed to getCIMIS
##' @author Matt Espe and Duncan Temple Lang
##'
##' @examples
##' #Get the station number for the Davis, CA station
##' # Fix later
##'
##' #Davis is station #6, and data starts 1982-07-17
##' ans = CIMISweather(startDate = "1987-07-17", endDate = Sys.Date(), targets = 6)
##'
CIMISweather = function(startDate, endDate, targets,
                         unitOfMeasure = 'M',
                         dataItems = character(),
                         prioritizeSCS = NA,
                         ...,
                         includeQC = FALSE,
                         appKey = getOption("Rcimis_key", stop("You need a key.")),
                         .opts = list())
{
    tmp = getCIMIS(appKey = appKey,
                    start = startDate,
                    end = endDate,
                    unitOfMeasure = unitOfMeasure,
                    dataItems = dataItems,
                    prioritizeSCS = prioritizeSCS,
                    targets = paste(targets, collapse = ","),
                    ..., .opts = .opts)

    if(!includeQC){
        idx = grepl('[.]Qc$|[.]Unit$', colnames(tmp))
        tmp = tmp[,!idx]
    }
 
    return(tmp)
}

