# These should not be documented or exported.

valid_opts <-
function(what = "Daily", opts = ValidOptions){  # match.arg()
    i = grep(what, names(opts))
    if(length(i) == 0)
        stop("what must be one of ", paste(names(opts), collapse = ", "))
    return(opts[[i]])
}



check_data_items <-
function(..., .args = list(...), opts = valid_opts())
{
    i <- match(.args, opts)
    if(any(is.na(i)))
        ## Supply proper option
        stop("Supplied API option is invalid: ",
             paste(.args[is.na(i)], collapse = ","))
}

check_opts <- function(args, opts = valid_opts()){
    ## Not a great fix for this
    i <- match(names(args), c(opts, "start","end","unitOfMeasure", "targets"))
    if(any(is.na(i)))
        ## Supply proper option
        stop("Supplied API option is invalid: ",
             paste(args[is.na(i)], collapse = ","))
    
    ## Might be better to have station specific dates?
    if(as.Date(args$start) < as.Date("1987-06-07") || as.Date(args$end) > Sys.Date())
        stop("Date out of range.")
    
    return(opts[i])
}

