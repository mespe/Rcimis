getDists <- function(lat, lon, n = 1, FUN = distHaversine){
    ## Only compare stations which are close (within a degree)
    close <- (diff(StnInfo$DdLatitude - lat) <= 1) & (diff(StnInfo$DdLongitude - lon) <= 1)
    
    d <- distm(rbind(StnInfo[close,c("DdLongitude","DdLatitude")], c(lon, lat)),
               fun = FUN)
    ## browser()
    ans <- d[nrow(d), -nrow(d)]
    i <- order(ans, decreasing = FALSE)
    
    return(data.frame(StnInfo[close,][i,][1:n,],
                      distKm = ans[i][1:n]/1000))
    }
