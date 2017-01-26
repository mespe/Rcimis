updateValidOptions =
function(filename = "ValidOpts.R")    
{
    tbls <- readHTMLTable("http://et.water.ca.gov/Rest/Index", stringsAsFactors = FALSE)[c("Daily Data Items", "Hourly Data Items")  ]
    items = lapply(tbls, `[[`, "Data Item")

    if(length(filename) && !is.na(filename) && filename != "") {
        txt = capture.output(dput(o))
        cat("ValidOptions <- ", txt, sep = "\n", file = filename)
    }
    items
}


