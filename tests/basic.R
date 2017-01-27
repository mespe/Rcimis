if(!is.na(getOption("Rcimis_key", NA))) {
 library(Rcimis)
 d = CIMISweather("2015-01-01",  "2015-01-20", 12)
 
 d = CIMISweather("2015-01-01",  "2015-01-20", 12, dataItems = c("day-air-tmp-min", "day-air-tmp-avg"))
}
