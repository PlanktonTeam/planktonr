## code to prepare `Marine Bioregions` dataset goes here

## Shrink data-file and save as internal data file
library(tidyverse)
library(rnaturalearth)
library(sf)
library(rmapshaper)

devtools::load_all() # Not sure if this is appropriate but I load the package to use the functions to load data

mbr <- sf::st_read(file.path("data-raw","marine_regions")) %>%  # Load marine regions as sf
  sf::st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  dplyr::select(-c(SHAPE_AREA, SHAPE_LEN)) %>%
  dplyr::filter(ENVIRON %in% "Marine") %>%
  dplyr::select(-c(OBJECTID, ENVIRON)) %>%
  sf::st_make_valid() %>%
  rmapshaper::ms_simplify(keep = 0.005)

so <- sf::st_read(file.path("data-raw","iho_SthnOcean","iho.shp")) %>%
  sf::st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  dplyr::select(name) %>%
  sf::st_make_valid() %>%
  sf::st_crop(c("xmin" = 85, "xmax" = 155, "ymin" = -85, "ymax" = -50))


mbr <- tibble(x = c(85, 85:155, 155, 85), y = c(-61, rep(-45, 71), -61, -61)) %>%
  as.matrix() %>%
  list() %>%
  sf::st_polygon() %>%
  sf::st_sfc(crs = 4326) %>%
  sf::st_as_sf() %>%
  dplyr::rename(geometry = x) %>%
  sf::st_union(so) %>%
  sf::st_make_valid() %>%
  st_difference(mbr[6,]) %>%
  dplyr::select(-REGION) %>%
  st_difference(mbr[8,]) %>%
  dplyr::select(-name) %>%
  dplyr::mutate(REGION = "Southern Ocean Region") %>%
  dplyr::bind_rows(mbr, .)

clr <- tibble::tribble( # Set1 on colorbrewer2.org
  ~REGION, ~Colour,
  "North", "#e41a1c",
  "Temperate East", "#377eb8",
  "North-west", "#4daf4a",
  "South-west", "#984ea3",
  "South-east", "#ff7f00",
  "Coral Sea",  "#ffff33",
  "Southern Ocean Region", "#a65628")


mbr <- dplyr::left_join(mbr, clr, by = "REGION") %>%
  dplyr::arrange("REGION")


MapOz <- rnaturalearth::ne_countries(scale = "small", country = "Australia",
                                     returnclass = "sf")

meta_sf <- pr_get_NRSTrips("Z") %>%
  dplyr::select("StationName", "StationCode", "Longitude", "Latitude") %>%
  dplyr::distinct() %>%
  dplyr::rename(Code = "StationCode",
                Station = "StationName") %>%
  dplyr::filter(Station != 'Port Hacking 4') %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


colNRS <- data.frame(Code = meta_sf$Code,
                     Colr = RColorBrewer::brewer.pal(9, "Set1")) %>%
  tibble::deframe()

colCPR <- mbr %>%
  sf::st_drop_geometry() %>%
  tibble::deframe()

usethis::use_data(mbr, MapOz, meta_sf, colNRS, colCPR, overwrite = TRUE, internal = TRUE, compress = "bzip2")

# tools::checkRdaFiles("R") # Check what compression to use above
# OK - bzip2
