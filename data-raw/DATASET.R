## code to prepare `Marine Bioregions` dataset goes here

## Shrink data-file and save as internal data file
library(tidyverse)
library(rnaturalearth)
library(sf)
library(rmapshaper)

devtools::load_all() # Not sure if this is appropriate but I load the package to use the functions to load data

sppSummary <- pr_get_SppCount() # Get the summary info

mbr <- sf::st_read(file.path("data-raw","marine_regions")) %>%  # Load marine regions as sf
  sf::st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  dplyr::select(-c(SHAPE_AREA, SHAPE_LEN)) %>%
  dplyr::filter(ENVIRON %in% "Marine") %>%
  sf::st_make_valid() %>%
  rmapshaper::ms_simplify(keep = 0.005)


clr <- tibble::tribble(
  ~REGION, ~Colour,
  "North", "#b3e2cd",
  "Temperate East", "#fdcdac",
  "North-west", "#cbd5e8",
  "South-west", "#f4cae4",
  "South-east", "#e6f5c9",
  "Coral Sea",  "#fff2ae")

mbr <- dplyr::left_join(mbr, clr, by = "REGION")

# imcra_pb <- sf::st_read(file.path("data-raw","imcra_4_pb")) %>%  # Load imcra provincial bioregions as sf
#   sf::st_transform(crs = "+proj=longlat +datum=WGS84") %>%
#   dplyr::select(-c(AREA_KM2)) %>%
#   sf::st_make_valid()

# imcra_meso <- sf::st_read(file.path("data-raw","imcra_4_meso")) %>%  # Load imcra mesoscale bioregions as sf
#   sf::st_transform(crs = "+proj=longlat +datum=WGS84") %>%
#   dplyr::select(-c(AREA_KM2, MESO_NUM, MESO_ABBR)) %>%
#   sf::st_make_valid()

MapOz <- rnaturalearth::ne_countries(scale = "small", country = "Australia",
                                       returnclass = "sf")

meta_sf <- pr_get_NRSTrips("Z") %>%
  dplyr::select(StationName, StationCode, Longitude, Latitude) %>%
  dplyr::distinct() %>%
  dplyr::rename(Code = StationCode, Station = StationName) %>%
  dplyr::filter(Station != 'Port Hacking 4') %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

usethis::use_data(mbr, MapOz, meta_sf, sppSummary, overwrite = TRUE, internal = TRUE, compress = "bzip2")

tools::checkRdaFiles("R") # Check what compression to use above
# OK - bzip2
