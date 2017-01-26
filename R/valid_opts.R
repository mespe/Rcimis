##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param what 
##' @param opts 
##' @return 
##' @author Matt Espe
##'
valid_opts <- function(what = "Daily", opts = ValidOptions){  # match.arg()
    i = grep(what, names(opts))
    if(length(i) == 0)
        stop(".....")
    return(opts[[i]])
        
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param args 
##' @param opts 
##' @return 
##' @author Matt Espe
##'
check_opts <- function(args, opts = valid_opts()){
    i <- match(args, opts)
    if(any(is.na(i)))
        ## Supply proper option
        stop("Supplied API option is invalid: ",
             paste(args[is.na(i)], collapse = ","))
    return(opts[i])
}

