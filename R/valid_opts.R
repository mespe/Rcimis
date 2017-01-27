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
    if(!all(sapply(.args, length) == 0)){
        i <- match(unlist(.args), opts[,"Data Item"])
        if(any(is.na(i)))
        ## Supply proper option
            stop("Supplied API option is invalid: ",
                 paste(.args[is.na(i)], collapse = ","))
    }
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

