# Add NRS station full names to data

Convert three-letter station codes to full station names for labelling
plots and tables. Useful for creating publication-ready figures with
descriptive location names.

## Usage

``` r
pr_add_StationName(df)
```

## Arguments

- df:

  A dataframe containing a `StationCode` column with NRS station codes

## Value

A dataframe with a new `StationName` column containing full station
names

## Details

### Station Code Mapping

The function converts the following codes to full names:

- `DAR` → "Darwin" (Northern Territory)

- `YON` → "Yongala" (Queensland, Great Barrier Reef)

- `NSI` → "North Stradbroke Island" (Queensland, near Brisbane)

- `PHB` → "Port Hacking" (New South Wales, Sydney)

- `MAI` → "Maria Island" (Tasmania, east coast)

- `KAI` → "Kangaroo Island" (South Australia)

- `ESP` → "Esperance" (Western Australia, south coast)

- `ROT` → "Rottnest Island" (Western Australia, Perth)

- `NIN` → "Ningaloo" (Western Australia, north coast)

- `VBM` → "Bonney Coast" (South Australia/Victoria border)

### Column Arrangement

The function adds a `StationName` column and positions it before
`StationCode` for logical ordering.

## See also

- [`pr_add_StationCode()`](https://planktonteam.github.io/planktonr/reference/pr_add_StationCode.md)
  for the reverse conversion

- [`pr_get_info()`](https://planktonteam.github.io/planktonr/reference/pr_get_info.md)
  for station metadata

## Examples

``` r
# Add station names to data with StationCode
df <- data.frame(StationCode = c("PHB", "MAI", "NSI")) %>%
  pr_add_StationName()
```
