library(XML)

updateValidOptions =
    #
    # This is not used directly in the package but is used
    # to generate the ValidOptions variable in the package
    # that is used by the valid_opts() function.
    # It reads the names of the valid parameters from the API documentation page
    # so that they are "up-to-date".  Run this function in this
    # directory of the package with
    #  updateValidOtions()
    # This regenerates the file ValidOpts.R in the R/ directory.
    #
function(filename = "../../R/ValidOpts.R")    
{
    tbls <- readHTMLTable("http://et.water.ca.gov/Rest/Index", stringsAsFactors = FALSE)#[c("Daily Data Items", "Hourly Data Items")  ]
    items = lapply(tbls, `[[`, "Data Item")

    if(length(filename) && !is.na(filename) && filename != "") {
        txt = capture.output(dput(o))
        cat("ValidOptions <- ", txt, sep = "\n", file = filename)
    }
    items
}


