
valid_opts <- function(what = "Daily", opts = ValidOptions){  # match.arg()
    i = grep(what, names(opts))
    if(length(i) == 0)
        stop(".....")
    return(opts[[i]])
        
}
