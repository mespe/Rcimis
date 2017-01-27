# These should not be documented or exported.  They are helper functions.


# Note that in the API documentation page, day-air-tmp-avg is repeated in the first two rows.

getDataItems =
function(what = "Data Items", info = ValidOptions[grepl(what, names(ValidOptions), ignore.case = TRUE)])
{
  unlist(unname(lapply(info, function(x) structure(x[[2]], names  = x[[1]]))))
}


matchDataItems <-
    #
    # This attempts to allow the caller to specify dataItems
    # using
    #  1) the value expected by the API, e.g. day-eto or partial match
    #  2) the human readable version, e.g. CIMIS Eto
    #  3) simply without the day- or hly- prefix.
    #
    # The text after the day- and hly- prefixes are not unique so there is some ambiguity for
    #  e.g. Dew Point which maps to day-dew-pnt and hly-dew-pnt
    #  a
    # getDataItems()[duplicated(gsub("^(day|hly)-", "", getDataItems()))]
    #
function(items, opts = getDataItems("Daily|Hourly"))
{
   if(!isParamValue(items))
      return(items)

   items = tolower(unique(items))
   opts = tolower(opts)
   
   ans = rep(NA, length(items))

   ok <- pmatch(items, opts)  # partial match.
   ans[!is.na(ok)] = opts[ok]

   w = which(is.na(ok))
   i <- pmatch(items[w], tolower(names(opts)))
   w = w[!is.na(i)]
   ans[ w ] = opts[ i[ !is.na(i)] ]

   w = is.na(ans)
   i = pmatch(items[w], gsub("^(day|hly)-", "", opts))
   ans[w[!is.na(i)]] = opts[i]
   
   
   if(any(is.na(i)))
        ## Supply proper option
      stop("Supplied API data item is invalid: ",
           paste(items[is.na(i)], collapse = ","))
   
   ans
}



isParamValue =
function(x)
   !(length(x) == 0 || is.na(x))


checkParams <-
function(..., .args = list(...))
{    
    ok = sapply(.args, isParamValue)
    .args = .args[ok]

     # Check the names are correct although already done in getCIMIS, but for ... use
    i <- match(names(.args), c("startDate","endDate","unitOfMeasure", "targets", "prioritizeSCS",
                                "dataItems"))
    if(any(is.na(i)))
        ## Supply proper option
        stop("Supplied API option is invalid: ",
             paste(names(.args)[is.na(i)], collapse = ","))

    if(!all(c("startDate", "endDate") %in% names(.args)))
        stop("must specify start and end date for query")

    if("unitOfMeasure" %in% names(.args)) {
       .args$unitOfMeasure = toupper(.args$unitOfMeasure)
       if(!(.args$unitOfMeasure %in% c("M", "E")))
          stop("unitOfMeasure must be either M or E")
    }

    if("prioritizeSCS" %in% names(.args)) {
       val = .args$prioritizeSCS
       if(is.logical(val))
          .args$prioritizeSCS = c("N", "Y")[ as.integer(val) + 1L]
       
       if(!(.args$prioritizeSCS %in% c("Y", "N")))
          stop("prioritizeSCS must be either Y or N")
    }    
        
    
    ## Might be better to have station specific dates?
    if(as.Date(.args$startDate) < as.Date("1987-06-07"))
        stop("startDate out of range.")
    if(as.Date(.args$endDate) > Sys.Date())
        stop("endDate out of range.")

    .args
    
}

# Unused

valid_opts <-
function(what = "Daily", opts = ValidOptions){  # match.arg()
    i = grep(what, names(opts))
    if(length(i) == 0)
        stop("what must be one of ", paste(names(opts), collapse = ", "))
    return(opts[[i]])
}

check_opts <- function(.args, opts = valid_opts()){
    ## API does not like empty fields
    i <- !sapply(.args, is.null)
    .args <- .args[i]
    ## Might be better to have station specific dates?
    if(as.Date(.args$start) < as.Date("1987-06-07") || as.Date(.args$end) > Sys.Date())
        stop("Date out of range.")

    return(.args)
}

check_data_items <-
function(..., .args = list(...), opts = valid_opts())
{
    if(!all(sapply(.args, length) == 0)){
        i <- match(unlist(.args), opts[,"Data Item"])
        if(any(is.na(i)))
        ## Supply proper option
            stop("Supplied API option is invalid: ",
                 paste(.args[is.na(i)], collapse = ","))
    }
}
