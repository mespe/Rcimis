updateStationInfo <- function(filename = "../data/StnInfo.rda"){
    ## Not sure how we want to handle this
    StnInfo <- get_station_info()
    save(StnInfo, file = filename)    
}


get_station_info <- function(){
    ##This gets the station info from the CIMIS site

    stations <- getURL('http://et.water.ca.gov/api/station')
    tmp <- fromJSON(stations)$Stations
    formatStnInfo(tmp)
}

formatStnInfo <- function(station_info){
    ## Fix some of the columns
    ## Lat/Long has colname Hms (Hours minutes seconds) but also contains decimal degree values
    ## Super hacky - fix later
    lat_tmp <- splitLatLon(station_info$HmsLatitude)
    lon_tmp <- splitLatLon(station_info$HmsLongitude)
    station_info$HmsLatitude <- lat_tmp[,1]
    station_info$DdLatitude <- lat_tmp[,2]
    station_info$HmsLongitude <- lon_tmp[,1]
    station_info$DdLongitude <- lon_tmp[,2]

    
    station_info$Elevation = as.numeric(station_info$Elevation)
    station_info$Elevation_m = station_info$Elevation * 0.3048
    station_info$ConnectDate = as.Date(station_info$ConnectDate, "%m/%d/%Y")
    station_info$DisconnectDate = as.Date(station_info$DisconnectDate, "%m/%d/%Y")

    return(station_info)
}

splitLatLon <- function(x){
    ans <- strsplit(x, " / ")
    ans <- as.data.frame(do.call(rbind, ans),
                         stringsAsFactors = FALSE)
    ans[,2] <- as.numeric(ans[,2])
    return(ans)
}
