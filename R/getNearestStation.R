##' Get geographic distance to nearest CIMIS stations
##'
##' This function gets the nearest n stations from a geographic point.
##' 
##' @title getDists
##' @param lat The latitude, in decimal degrees
##' @param lon longitude, in decimal degrees
##' @param n number of nearest stations to return
##' @param FUN the function used to calculate the distance. Passed to the \code{distm} function in the \code{geospheres} package
##' @param cached logical, should the cached station information be used, or should a up to the date version be retrieved from CIMIS
##' @param StnInfo object providing details of the CIMIS weather stations. This can be retrieved
##' using \code{getStationInfo} or \code{updateStationInfo}.
##' @return a subset of \code{StnInfo} ordered by distance to the specificed point, with the distance in Km added
##' @author Matt Espe
##'
##' @examples
##' #Get the 5 closest stations
##' getDists(37.8, -121.22, n = 5)
##' 
getDists = function(lat, lon, n = 1, FUN = distHaversine, cached = TRUE, StnInfo = getStationInfo(cached)){
    ## Only compare stations which are close (within a degree)
    close = (diff(StnInfo$DdLatitude - lat) <= 1) & (diff(StnInfo$DdLongitude - lon) <= 1)
    
    d = distm(rbind(StnInfo[close,c("DdLongitude","DdLatitude")], c(lon, lat)),
               fun = FUN)
    ## browser()
    ans = d[nrow(d), -nrow(d)]
    i = order(ans, decreasing = FALSE)
    
    return(data.frame(StnInfo[close,][i,][1:n,],
                      distKm = ans[i][1:n]/1000))
    }
