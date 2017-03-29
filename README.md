# Rcimis

A packages for interacting with the CIMIS API from R.

###API keys

You will need an API key to use this package. You can apply for one
for free. Please follow the directions at:
http://et.water.ca.gov/Home/Register/

*Make certain you follow through to activate your CIMIS key
(upper-right corner of the CIMIS page).

###Installation

The easiest options are:

```
library(devtools)
install_github("Rcimis", "mespe")
```

###Use

The package currently looks in the users R options for the API key,
which allows the user to store the key without having to re-enter it
with each call or possibilty expose the key in an R script.

The function CIMISweather accesses the CIMIS API, and follows the REST
protocols outlined at http://et.water.ca.gov/Rest/Index

```
CIMISweather(startDate = "2015-01-01", endDate = "2015-02-01", targets
= c(6,32))
```

Additionally, a convience function to calculate the geographic
distance from a user specified lat/long to the nearest stations can be
returned with getDist.

```
getDists(38.5, -121.5)
    StationNbr  Name  City              RegionalOffice County ConnectDate
121        121 Dixon Dixon North Central Region Office Solano  1994-09-20
    DisconnectDate IsActive IsEtoStation Elevation GroundCover HmsLatitude
121     2030-12-31     True         True        37       Grass   38º24'56N
    HmsLongitude ZipCodes SitingDesc DdLatitude DdLongitude Elevation_m
121  -121º47'13W    95620              38.41556   -121.7869     11.2776
      distKm
121 26.71803
```
