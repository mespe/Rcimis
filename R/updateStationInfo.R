updateStationInfo = function(filename = getOption("CIMIS_STATION_FILE", system.file("StationInfo.rds", package = "Rcimis"))) {
    ## Not sure how we want to handle this
    StnInfo = getStationInfo()
    # only if check permissions.
    saveRDS(StnInfo, file = filename)
    StnInfo
}

getStationInfo = function(...){
    ##This gets the station info from the CIMIS site
    stations = getURL('http://et.water.ca.gov/api/station', ...)
    tmp = fromJSON(stations)$Stations
    formatStnInfo(tmp)
}

formatStnInfo = function(stationInfo){
    ## Fix some of the columns
    ## Lat/Long has colname Hms (Hours minutes seconds) but also contains decimal degree values
    ## Super hacky - fix later
    lat_tmp = splitLatLon(stationInfo$HmsLatitude)
    lon_tmp = splitLatLon(stationInfo$HmsLongitude)
    stationInfo$HmsLatitude = lat_tmp[,1]
    stationInfo$DdLatitude = lat_tmp[,2]
    stationInfo$HmsLongitude = lon_tmp[,1]
    stationInfo$DdLongitude = lon_tmp[,2]

    stationInfo$Elevation = as.numeric(stationInfo$Elevation)
    stationInfo$Elevation_m = stationInfo$Elevation * 0.3048
    stationInfo$ConnectDate = as.Date(stationInfo$ConnectDate, "%m/%d/%Y")
    stationInfo$DisconnectDate = as.Date(stationInfo$DisconnectDate, "%m/%d/%Y")

    return(stationInfo)
}

splitLatLon = function(x){
    ## Simple function to split the lat and lon columns
    ## To Hms and DD
    ans = strsplit(x, " / ")
    ans = as.data.frame(do.call(rbind, ans),
                         stringsAsFactors = FALSE)
    ans[,2] = as.numeric(ans[,2])
    return(ans)
}
