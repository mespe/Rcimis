## Key is stored separate and not shared

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
##'
##' #Davis is station #6, and data starts 1982-07-17
##' if(!is.null(getOption("Rcimis_key")))
##'     ans = CIMISweather(startDate = "1987-07-17", endDate = Sys.Date(), targets = 6)
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

