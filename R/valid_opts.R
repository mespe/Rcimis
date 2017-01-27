#!!! These should not be documented or exported.

valid_opts <-
function(what = "Daily", opts = ValidOptions){  # match.arg()
    i = grep(what, names(opts))
    if(length(i) == 0)
        stop(".....")
    return(opts[[i]])
        
}


check_opts <- function(..., .args = list(...), opts = valid_opts()){
    i <- match(.args, opts)
    if(any(is.na(i)))
        ## Supply proper option
        stop("Supplied API option is invalid: ",
             paste(.args[is.na(i)], collapse = ","))
    return(opts[i])
}

