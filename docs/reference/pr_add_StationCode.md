# Add NRS station codes to data

Convert station names or trip codes to three-letter station codes.
Useful for standardising location identifiers in datasets that use full
names or for extracting station information from trip codes.

## Usage

``` r
pr_add_StationCode(df)
```

## Arguments

- df:

  A dataframe containing either:

  - `StationName` column with full station names, or

  - `TripCode` column where first 3 characters are the station code

## Value

A dataframe with a new `StationCode` column

## Details

### Station Name Conversion

The function converts full names to three-letter codes:

- "Darwin" → `DAR`

- "Yongala" → `YON`

- "North Stradbroke Island" or "North Stradbroke" → `NSI`

- "Port Hacking" → `PHB`

- "Maria Island" → `MAI`

- "Kangaroo Island" → `KAI`

- "Esperance" → `ESP`

- "Rottnest Island" → `ROT`

- "Ningaloo" → `NIN`

- "Bonney Coast" → `VBM`

- "Southern Ocean Time Series" → `SOTS`

### Trip Code Extraction

If the dataframe contains `TripCode` but not `StationName`, the function
extracts the first three characters as the station code. NRS trip codes
follow the format `XXX######` where `XXX` is the station code.

### Column Arrangement

The function adds a `StationCode` column and positions it after
`StationName` (if present) or `TripCode` for logical ordering.

## See also

- [`pr_add_StationName()`](https://planktonteam.github.io/planktonr/reference/pr_add_StationName.md)
  for the reverse conversion

- [`pr_get_info()`](https://planktonteam.github.io/planktonr/reference/pr_get_info.md)
  for station metadata

- [`pr_get_trips()`](https://planktonteam.github.io/planktonr/reference/pr_get_trips.md)
  which returns data with trip codes

## Examples

``` r
# Add station codes from station names
df <- data.frame(StationName = c("Port Hacking", "Maria Island")) %>%
  pr_add_StationCode()

# Extract station codes from trip codes
df <- data.frame(TripCode = c("PHB20220101", "MAI20220115")) %>%
  pr_add_StationCode()
```
