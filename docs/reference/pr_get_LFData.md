# Get NRS larval fish abundance data

Load and format larval fish count data from NRS biogeochemistry
sampling. Data includes counts and abundance (standardised to 1000 m³)
of fish larvae identified to family or species level.

## Usage

``` r
pr_get_LFData()
```

## Value

A dataframe with columns:

- **StationName**: NRS station name

- **Latitude**, **Longitude**: Sample location

- **TripCode**: Unique voyage identifier

- **SampleTime_UTC**, **SampleTime_Local**: Sampling time

- **Year_Local**, **Month_Local**, **Day_Local**: Date components

- **SampleDepth_m**: Maximum depth of tow

- **Temperature_degC**, **Salinity_psu**: Surface environmental data

- **Volume_m3**: Volume of water filtered (m³)

- **Vessel**: Research vessel name

- **TowType**: Vertical or oblique

- **GearMesh_um**: Net mesh size (micrometres)

- **Bathymetry_m**: Bottom depth at station

- **Species**: Coded species name with WoRMS ID

- **Species2**: Formatted species name for display

- **Count**: Number of larvae in sample

- **Abundance_1000m3**: Larvae per 1000 m³

- **QC_flag**: Data quality flag (1-4)

## Details

### Data Collection

Larval fish samples are collected at NRS stations using:

- Vertical net tows through the water column

- Various mesh sizes (typically 100-500 µm)

- Sample volumes calculated from net dimensions and tow depth

Fish larvae are identified to the lowest possible taxonomic level,
typically family but sometimes genus or species for distinctive forms.

### Data Format

The function returns data in long format with:

- One row per species per sample

- Raw counts and standardised abundance (per 1000 m³)

- Environmental data (temperature, salinity)

- Sampling metadata (mesh size, volume filtered, tow type)

### Species Names

The `Species` column contains coded names (e.g.,
"Acanthuridae_37437900") combining family name and WoRMS taxonomic ID.
The `Species2` column provides formatted names for display purposes.

### Taxonomic Codes

Family names in the data typically end with "idae" (e.g., Acanthuridae
for surgeonfishes, Carangidae for trevallies). Codes include:

- Family name

- WoRMS AphiaID (taxonomic identifier)

- Life stage code (3 = larval stage)

### Data Availability

Larval fish sampling is conducted opportunistically during NRS voyages.
Not all stations or months have larval fish data.

### Quality Control

The `QC_flag` column indicates data quality:

- `1` = Good data

- `2` = Probably good data

- `3` = Probably bad data (use with caution)

- `4` = Bad data (do not use)

## See also

- [`pr_get_NRSData()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSData.md)
  for plankton data from the same stations

## Examples

``` r
# Load larval fish data
dat <- pr_get_LFData()

# Check which families are most common
dat %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(TotalCount = sum(Count, na.rm = TRUE)) %>%
  dplyr::arrange(dplyr::desc(TotalCount))
#> --- planktonr_dat Attributes ---
#>   Type: Fish
#>   Survey: NRS
#> 
#> # A tibble: 240 × 2
#>    Species                                      TotalCount
#>    <chr>                                             <dbl>
#>  1 Myctophidae_Myctophidae_37122000                  82487
#>  2 Clupeidae_Sardinops.sagax_37085002                49015
#>  3 Gonorynchidae_Gonorynchus.greyi_37141001          23047
#>  4 Carangidae_Trachurus.novaezelandiae_37337003      22892
#>  5 Bothidae_Bothidae_37460922                        19145
#>  6 Carangidae_Carangidae_37337000                    17071
#>  7 Labridae_Labridae_37384000                        16646
#>  8 Carangidae_Pseudocaranx.georgianus_37337062       16175
#>  9 Engraulidae_Engraulis.australis_37086001          13735
#> 10 Gonostomatidae_Gonostomatidae_37106912            12542
#> # ℹ 230 more rows

# Abundance at Maria Island
dat %>%
  dplyr::filter(StationName == "Maria Island",
                QC_flag == 1) %>%
  dplyr::group_by(Year_Local, Species2) %>%
  dplyr::summarise(MeanAbundance = mean(Abundance_1000m3, na.rm = TRUE),
                   .groups = "drop")
#> --- planktonr_dat Attributes ---
#>   Type: Fish
#>   Survey: NRS
#> 
#> # A tibble: 1,920 × 3
#>    Year_Local Species2                                      MeanAbundance
#>         <dbl> <chr>                                                 <dbl>
#>  1       2014 Acanthuridae: Acanthuridae (37437900)                     0
#>  2       2014 Acropomatidae: Acropomatidae (37311956)                   0
#>  3       2014 Acropomatidae: Synagrops spp (37311949)                   0
#>  4       2014 Acropomatidae: Verilus anomalus (37311053)                0
#>  5       2014 Ambassidae: Ambassidae (37310900)                         0
#>  6       2014 Ambassidae: Ambassis jacksoniensis (37310012)             0
#>  7       2014 Ambassidae: Ambassis marianus (37310018)                  0
#>  8       2014 Ammodytidae: Ammodytidae (37425000)                       0
#>  9       2014 Ammodytidae: Ammodytoides spp (37425901)                  0
#> 10       2014 Antennariidae: Antennariidae (37210915)                   0
#> # ℹ 1,910 more rows
```
