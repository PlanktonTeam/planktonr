## code to prepare `Marine Bioregions` dataset goes here

## Shrink data-file and save as internal data file
library(tidyverse)

mbr <- sf::st_read(file.path("data-raw","marine_regions")) %>%  # Load marine regions as sf
  sf::st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  dplyr::select(-c(SHAPE_AREA, SHAPE_LEN)) %>%
  dplyr::filter(ENVIRON %in% "Marine") %>%
  sf::st_make_valid()

imcra_pb <- sf::st_read(file.path("data-raw","imcra_4_pb")) %>%  # Load imcra provincial bioregions as sf
  sf::st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  dplyr::select(-c(AREA_KM2)) %>%
  sf::st_make_valid()

imcra_meso <- sf::st_read(file.path("data-raw","imcra_4_meso")) %>%  # Load imcra mesoscale bioregions as sf
  sf::st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  dplyr::select(-c(AREA_KM2, MESO_NUM, MESO_ABBR)) %>%
  sf::st_make_valid()

usethis::use_data(mbr, imcra_pb, imcra_meso, overwrite = TRUE, internal = TRUE, compress = "bzip2")

tools::checkRdaFiles("R") # Check what compression to use above
# OK - bzip2