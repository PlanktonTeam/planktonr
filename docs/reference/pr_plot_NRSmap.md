# Create map showing selected NRS station locations

Plot Australian coastline with NRS sampling stations, highlighting
selected stations in red and non-selected stations in blue. Useful for
showing which stations are included in an analysis or visualisation.

## Usage

``` r
pr_plot_NRSmap(sites, Survey = "NRS", Type = "Zooplankton")
```

## Arguments

- sites:

  Character vector of station codes to highlight. Valid codes:

  - `"DAR"` - Darwin

  - `"YON"` - Yongala

  - `"NSI"` - North Stradbroke Island

  - `"PHB"` - Port Hacking

  - `"MAI"` - Maria Island

  - `"KAI"` - Kangaroo Island

  - `"ESP"` - Esperance

  - `"ROT"` - Rottnest Island

  - `"NIN"` - Ningaloo

- Survey:

  Which station network to display:

  - `"NRS"` - All National Reference Stations (default)

  - `"LTM"` - Long Term Monitoring subset (Maria Island, Port Hacking,
    Rottnest Island)

  - `"Coastal"` - Coastal stations

- Type:

  Plankton type, affecting which stations appear:

  - `"Zooplankton"` - Standard NRS stations (default)

  - `"Phytoplankton"` - Includes Southern Ocean Time Series (SOTS)
    station

## Value

A ggplot2 object with transparent background, suitable for overlaying or
saving with
[`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)

## Details

### Map Extent

The map shows the Australian coastline from:

- Longitude: 112.8°E to 154.5°E

- Latitude: -44°S to -10.5°S (or -50°S if including SOTS)

### Visual Design

- **Red points**: Selected stations (specified in `sites` argument)

- **Blue points**: Non-selected stations

- Grey land mass

- Transparent background for easy integration into documents

### Use Cases

This function is particularly useful for:

- Sidebar panels in Shiny applications

- Figure panels showing study site locations

- Publication maps indicating data sources

- Educational materials showing NRS network coverage

### SOTS Station

The Southern Ocean Time Series (SOTS) station south of Tasmania is only
included when `Type = "Phytoplankton"` as zooplankton sampling there is
infrequent or uses different methods.

## See also

- [`pr_get_info()`](https://planktonteam.github.io/planktonr/reference/pr_get_info.md)
  for station metadata

- [`pr_plot_CPRmap()`](https://planktonteam.github.io/planktonr/reference/pr_plot_CPRmap.md)
  for CPR bioregion maps

## Examples

``` r
# Map showing Maria Island and Port Hacking
sites <- c("MAI", "PHB")
pmap <- pr_plot_NRSmap(sites, Type = "Phytoplankton")
print(pmap)


# Long Term Monitoring stations with one highlighted
pr_plot_NRSmap(sites = "MAI", Survey = "LTM")

```
