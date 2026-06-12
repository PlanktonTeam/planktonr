# Get sampling trip metadata

Load metadata for sampling trips from the National Reference Stations
(NRS) or Continuous Plankton Recorder (CPR) survey. This unified
function replaces the survey-specific functions
[`pr_get_NRSTrips()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSTrips.md)
and
[`pr_get_CPRTrips()`](https://planktonteam.github.io/planktonr/reference/pr_get_CPRTrips.md).

## Usage

``` r
pr_get_trips(Survey = "NRS", ...)
```

## Arguments

- Survey:

  Survey type to retrieve trips for:

  - `"NRS"` - National Reference Station trips (default)

  - `"CPR"` - Continuous Plankton Recorder trips

- ...:

  Additional arguments passed to
  [`pr_add_Bioregions()`](https://planktonteam.github.io/planktonr/reference/pr_add_Bioregions.md)
  when `Survey = "CPR"`. Currently supports:

  - `near_dist_km` - Distance in kilometres to pad bioregion boundaries
    when assigning samples to regions (default behaviour uses exact
    boundaries)

## Value

A dataframe with trip information. Common columns include:

- `TripCode` - Unique trip identifier

- `SampleTime_Local` - Local sampling date and time

- `Year_Local`, `Month_Local` - Temporal components

- `Latitude`, `Longitude` - Geographic coordinates

Additional columns vary by survey:

- **NRS:** `StationCode`, `StationName`

- **CPR:** `BioRegion` - Australian marine bioregion assignment

## Details

**NRS Trips:** Returns information about each NRS sampling trip
(voyage), including the trip code, station, date/time, and basic
metadata. Only NRS project trips are included (SOTS trips are excluded).
Samples designated as 'P' (plankton) samples or with no sample type
designation are included.

**CPR Trips:** The CPR samples continuously as it is towed behind ships
of opportunity. Each "trip" represents a segment of the tow, typically
covering approximately 3 nautical miles. Samples are automatically
assigned to Australian marine bioregions using
[`pr_add_Bioregions()`](https://planktonteam.github.io/planktonr/reference/pr_add_Bioregions.md).
Bioregions include: Temperate East, South-east, South-west, North, and
North-west.

## See also

[`pr_get_info()`](https://planktonteam.github.io/planktonr/reference/pr_get_info.md)
for station-level metadata,
[`pr_add_Bioregions()`](https://planktonteam.github.io/planktonr/reference/pr_add_Bioregions.md)
for bioregion assignment details

## Examples

``` r
# Get NRS trip metadata
dat <- pr_get_trips(Survey = "NRS")

# Get CPR trip metadata with default bioregion assignment
dat <- pr_get_trips(Survey = "CPR")

# Get CPR trips with expanded bioregion boundaries (250 km padding)
dat <- pr_get_trips(Survey = "CPR", near_dist_km = 250)

# Get HAB trip metadata
dat <- pr_get_trips(Survey = "HAB")

# Examine sampling frequency by station (NRS)
dat <- pr_get_trips(Survey = "NRS")
table(dat$StationName, dat$Year_Local)
#>                          
#>                           2002 2003 2004 2005 2006 2007 2008 2009 2010 2011
#>   Bonney Coast               0    0    0    0    0    0    0    0    0    0
#>   Darwin                     0    0    0    0    0    0    0    0    0    2
#>   Esperance                  0    0    0    0    0    0    0    4    3    4
#>   Kangaroo Island            0    0    0    0    0    0    8    9    9   11
#>   Maria Island               0    0    0    0    0    0    0    9    9   10
#>   Ningaloo                   0    0    0    0    0    0    0    0    1    4
#>   North Stradbroke Island    0    0    0    0    0    0    4   15   12   12
#>   Port Hacking               0    0    0    0    0    0    0   13   13   12
#>   Port Hacking 4            11   12   12    9    5   11   11    1    0    0
#>   Rottnest Island            0    0    0    0    0    0    0    2    9   11
#>   Yongala                    0    0    0    0    0    0    0    3    9   11
#>                          
#>                           2012 2013 2014 2015 2016 2017 2018 2019 2020 2021
#>   Bonney Coast               0    0    0    0    0    0    0    0    0    0
#>   Darwin                    14    6   11   11   10   10   10   10   11   10
#>   Esperance                  5    2    0    0    0    0    0    0    0    0
#>   Kangaroo Island            3    4    7    6    4    4    4    4    3    4
#>   Maria Island              11   11   13    9   10   11   10   10    9   12
#>   Ningaloo                   4    3    0    0    0    0    0    0    0    0
#>   North Stradbroke Island    9   12   11   12   10   11   10   10   10   11
#>   Port Hacking              10   12   12   10   10   10   10   11    9    9
#>   Port Hacking 4             0    0    0    0    0    0    0    0    0    0
#>   Rottnest Island           11   10   11   10   11   12   12   12   11   11
#>   Yongala                   12   11   12   12   12   12   12   12   12   12
#>                          
#>                           2022 2023 2024 2025 2026
#>   Bonney Coast               0    0    3    4    2
#>   Darwin                    10   10   10    6    1
#>   Esperance                  0    0    0    0    0
#>   Kangaroo Island            4    3    4    4    1
#>   Maria Island              11   10   11   12    5
#>   Ningaloo                   0    0    0    0    0
#>   North Stradbroke Island   10   12    8    7    2
#>   Port Hacking              11    9   10   10    4
#>   Port Hacking 4             0    0    0    0    0
#>   Rottnest Island           10   11   10   12    4
#>   Yongala                   11   13   12   12    2

# Examine sampling effort by bioregion and year (CPR)
dat <- pr_get_trips(Survey = "CPR")
table(dat$BioRegion, dat$Year_Local)
#>                        
#>                         2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017
#>   Coral Sea                0    0    0    0    0   34   13  204  285    0    0
#>   None                     0    0    4  216  282  468  434  493  456  352  283
#>   North                    0    0    0    0    0    0  142    0    0    0   55
#>   North-west               0    0    0  116   78  452  107    0    0    0    0
#>   South-east               0   43  295  770  791  828  723  702  915  630  934
#>   South-west               0    0    0  255  272  313  250  441  323  172  311
#>   Southern Ocean Region  575  588  488  552  431  215  117  110  130  432  655
#>   Temperate East           0    0  475  518  266  346   96  809  686  231  454
#>                        
#>                         2018 2019 2020 2021 2022 2023 2024 2025 2026
#>   Coral Sea                0   10    0    0    0    0    0    0    0
#>   None                   140  876  376  373  332  547  413  304   99
#>   North                    0   61    0    0   23    0    0    0    0
#>   North-west               0   34    0    0  187    0    0    0    0
#>   South-east             952  852  584  279  306  212  509  595   65
#>   South-west              83   54    5    0   95  140  116   40    0
#>   Southern Ocean Region 1277  909   23  848    0  112  107  120    0
#>   Temperate East         675  471  406  225  325  341  295  328   79
```
