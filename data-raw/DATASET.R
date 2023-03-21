## code to prepare `Marine Bioregions` dataset goes here

## Shrink data-file and save as internal data file
library(tidyverse)
# library(rnaturalearth)
# library(sf)
# library(rmapshaper)

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
  sf::st_difference(mbr[6,]) %>%
  dplyr::select(-REGION) %>%
  sf::st_difference(mbr[8,]) %>%
  dplyr::select(-name) %>%
  dplyr::mutate(REGION = "Southern Ocean Region") %>%
  dplyr::bind_rows(mbr, .)

mbr[(dim(mbr)[1])+1,"REGION"] <- "None" # Add an empty geometry for the points that are not in a bioregion.

clr <- tibble::tribble( # Set1 on colorbrewer2.org - Yellow has been updated.
  ~REGION, ~Colour,
  "North", "#e41a1c",
  "Temperate East", "#377eb8",
  "North-west", "#4daf4a",
  "South-west", "#984ea3",
  "South-east", "#ff7f00",
  "Coral Sea",  "#fce205",
  "Southern Ocean Region", "#a65628",
  "None", "#808080")


mbr <- dplyr::left_join(mbr, clr, by = "REGION") %>%
  dplyr::arrange("REGION")

MapOz <- rnaturalearth::ne_countries(scale = "small", country = "Australia",
                                     returnclass = "sf")

# Details for coastal stations
CSCodes <- tibble::tibble(StationName = c("Balls Head", "Salmon Haul", "Bare Island", "Cobblers Beach", "Towra Point", "Lilli Pilli",
                                          "Derwent Estuary B1", "Derwent Estuary B3", "Derwent Estuary E", "Derwent Estuary G2",
                                          "Derwent Estuary KB", "Derwent Estuary RBN", "Derwent Estuary U2", "Low Head",
                                          "Tully River Mouth mooring", "Russell-Mulgrave River mooring", "Green Island", "Port Douglas",
                                          "Cape Tribulation", "Double Island", "Yorkey's Knob", "Fairlead Buoy", "Hobsons; Port Phillip Bay",
                                          "Long Reef; Port Phillip Bay", "Geoffrey Bay", "Channel", "Pioneer Bay", "Centaur Reef",
                                          "Wreck Rock", "Inshore reef_Channel", "Inshore reef_Geoffrey Bay"),
                          StationCode = c("BAH", "SAH", "BAI", "COB", "TOP", "LIP", "DEB", "DES", "DEE", "DEG", "DEK", "DER", "DEU", "LOH",
                                     "TRM", "RMR", "GNI", "PTD", "CTL", "DBI", "YKK", "FLB", "HOB", "LOR", "GEB", "CHA", "PIB", "CER",
                                     "WRR", "IRC", "IGB"),
                          State = factor(c("NSW", "NSW", "NSW", "NSW", "NSW", "NSW", "TAS", "TAS","TAS","TAS","TAS","TAS","TAS", "TAS", "GBR",
                                    "GBR","GBR","GBR","GBR","GBR","GBR","GBR", "VIC", "VIC","GBR","GBR","GBR","WA", "WA","GBR","GBR" ),
                                    levels = c("GBR", "NSW", "WA", "VIC", "TAS"))) %>%
  dplyr::arrange(State)

# NRS input into pl_plot_NRSmap()
meta_sf <- planktonr::pr_get_NRSTrips("Z") %>%
  dplyr::select("StationName", "StationCode", "Longitude", "Latitude") %>%
  dplyr::distinct() %>%
  dplyr::rename(Code = "StationCode",
                Station = "StationName") %>%
  dplyr::filter(Station != 'Port Hacking 4') %>%
  dplyr::arrange(desc(Latitude)) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Microbial Coastal station input into pl_plot_NRSmap()
csDAT <- planktonr::pr_get_NRSMicro("Coastal") %>%
  dplyr::select("StationName", "StationCode", "Longitude", "Latitude", "State") %>%
  dplyr::rename(Code = "StationCode",
                Station = "StationName") %>%
  dplyr::group_by(Code, Station, State) %>%
  dplyr::summarise(Latitude = mean(Latitude, na.rm = TRUE),
                   Longitude = mean(Longitude, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::distinct() %>%
  dplyr::arrange(desc(State), desc(Latitude)) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# https://coolors.co/palette/d00000-ffba08-cbff8c-8fe388-1b998b-3185fc-5d2e8c-46237a-ff7b9c-ff9b85
# Darwin                 Yongala                Ningaloo      North Stradbroke Island         Rottnest Island               Esperance            Port Hacking         Kangaroo Island            Maria Island
 # "#ff8500"               "#b66ee8"             "#ff9b85"           "#d00000".                 "#46237a"               "#1b998b"                  "#8fe388"                 "#ff7b9c"            "#3185fc"

coolor <- c("#ff8500", "#b66ee8", "#ff9b85", "#d00000", "#46237a", "#1b998b", "#8fe388", "#ff7b9c", "#3185fc") # "#cbff8c" "#ffba08"

colNRSCode <- data.frame(Code = meta_sf$Code,
                         Colr = coolor) %>%
  tibble::deframe()

colNRSName <- data.frame(Code = meta_sf$Station,
                         Colr = coolor) %>%
  tibble::deframe()

pchNRSName <- data.frame(Code = meta_sf$Station,
                         pchr = rep(16,9)) %>%
  tibble::deframe()

pchNRSCode <- data.frame(Code = meta_sf$Code,
                         pchr = rep(16,9)) %>%
  tibble::deframe()

ltyNRSCode <- data.frame(Code = meta_sf$Code,
                         pchr = rep("solid",9)) %>%
  tibble::deframe()

ltyNRSName <- data.frame(Code = meta_sf$Station,
                         pchr = rep("solid",9)) %>%
  tibble::deframe()

colCPR <- mbr %>%
  sf::st_drop_geometry() %>%
  tibble::deframe()

## Coastal station colours
stateCol <- c(rep("#8FE388", 5), rep("#1B998B", 4), rep("#CBFF8C", 4), rep("#FFBA08",6), rep("#3185FC",2), rep("#5D2E8C",2), rep("#FF7B9C",4), rep("#FF9B85", 4))
stateLTY <- c("solid", "dashed", "dotted", "dotdash", "longdash", "solid", "dashed", "dotted", "dotdash",  "solid", "dashed", "dotted", "dotdash",
              "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "solid", "dashed", "solid", "dashed", "solid", "dashed", "dotted",
              "dotdash", "solid", "dashed", "dotted", "dotdash")
statePCH <- c(seq(0, 25, 1), 0, 1, 2, 3, 4)

colCSCode <- data.frame(Code = CSCodes$StationCode,
                         Colr = stateCol) %>%
  tibble::deframe()

colCSName <- data.frame(Code = CSCodes$StationName,
                        Colr = stateCol) %>%
  tibble::deframe()

pchCSCode <- data.frame(Code = CSCodes$StationCode,
                        Colr = statePCH) %>%
  tibble::deframe()

pchCSName <- data.frame(Code = CSCodes$StationName,
                        Colr = statePCH) %>%
  tibble::deframe()

ltyCSCode <- data.frame(Code = CSCodes$StationCode,
                        pchr = stateLTY) %>%
  tibble::deframe()

ltyCSName <- data.frame(Code = CSCodes$StationName,
                        pchr = stateLTY) %>%
  tibble::deframe()

colNRSCode <- c(colNRSCode, colCSCode)
colNRSName <- c(colNRSName, colCSName)
pchNRSCode <- c(pchNRSCode, pchCSCode)
pchNRSName <- c(pchNRSName, pchCSName)
ltyNRSCode <- c(ltyNRSCode, ltyCSCode)
ltyNRSName <- c(ltyNRSName, ltyCSName)

rm(colCSCode, colCSName, pchCSCode, pchCSName, ltyCSCode, ltyCSName, stateCol, stateLTY, statePCH)

# CPR policy info
CPRinfo <- planktonr::pr_get_PolicyInfo("CPR")

usethis::use_data(mbr, MapOz, meta_sf, csDAT, colCPR, CPRinfo, CSCodes,
                  colNRSCode, colNRSName, pchNRSName, pchNRSCode, ltyNRSCode, ltyNRSName,
                  overwrite = TRUE, internal = TRUE, compress = "bzip2")

# tools::checkRdaFiles("R") # Check what compression to use above
# OK - bzip2
