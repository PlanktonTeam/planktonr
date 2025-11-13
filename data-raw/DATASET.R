## code to prepare `Marine Bioregions` dataset goes here

## Shrink data-file and save as internal data file
library(tidyverse)


MapOz <- rnaturalearth::ne_countries(scale = "small", country = "Australia",
                                     returnclass = "sf")



# Continuous Plankton Recorder (CPR) --------------------------------------

# CPR policy info
CPRinfo <- planktonr::pr_get_PolicyInfo("CPR")

mbr <- sf::st_read(file.path("data-raw","marine_regions")) %>% # Load marine regions as sf
  sf::st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  dplyr::select(-c("SHAPE_AREA", "SHAPE_LEN")) %>%
  dplyr::filter(ENVIRON %in% "Marine") %>%
  dplyr::select(-c("OBJECTID", "ENVIRON")) %>%
  sf::st_make_valid() %>%
  rmapshaper::ms_simplify(keep = 0.005)

so <- sf::st_read(file.path("data-raw","iho_SthnOcean","iho.shp")) %>%
  sf::st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  dplyr::select("name") %>%
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
  dplyr::select(-"REGION") %>%
  sf::st_difference(mbr[8,]) %>%
  dplyr::select(-"name") %>%
  dplyr::mutate(REGION = "Southern Ocean Region") %>%
  dplyr::bind_rows(mbr, .)

mbr[(dim(mbr)[1])+1,"REGION"] <- "None" # Add an empty geometry for the points that are not in a bioregion.

# one example from http://vrl.cs.brown.edu/color
clr <- tibble::tribble( #
  ~REGION, ~Colour,
  "North", "#4081ec",
  "Temperate East", "#dc8873",
  "North-west", "#86b5a1",
  "South-west", "#952c3d",
  "South-east", "#36c272",
  "Coral Sea", "#372a4e",
  "Southern Ocean Region", "#8da83e",
  "None", "#808080")


mbr <- dplyr::left_join(mbr, clr, by = "REGION") %>%
  dplyr::arrange("REGION")


colCPR <- mbr %>%
  sf::st_drop_geometry() %>%
  tibble::deframe()

pchCPR <- data.frame(Code = mbr$REGION,
                     pchr = rep(16, 11)) %>%
  tibble::deframe()

ltyCPR <- data.frame(Code = mbr$REGION,
                     pchr = rep("solid", 11)) %>%
  tibble::deframe()


## Load Southern Ocean CPR -----

library(planktonr)

# dat <- planktonr::pr_get_Raw("cpr_derived_indices_data")

cpr_AAD <- read_csv("data-raw/AADC-00099_-_2025_data_update/AADC-00099_29August2025.csv",
                    col_types = cols(
                      Time = col_character(),
                      Date = col_character())) %>%
  mutate(SampleTime_UTC = lubridate::dmy_hm(paste(Date, Time)), # Merge Date and Time into datetime
         TripCode = paste0("AADX", format(SampleTime_UTC, "%Y%m%d")), # Create TripCode to match AusCPR
         Sample_ID = paste(Tow_Number, `Segment_No.`, sep = "-"), # Create Sample_ID to match AusCPR
         SampleTime_Local = SampleTime_UTC + lubridate::hours(round(Longitude / 15)), # Convert to local time by adding the longitude hours offset
         SampleVolume_m3 = Segment_Length * 1852 * 0.0127^2, # Convert to volume
         Phytoplankton_Colour_Index = as.numeric(Phytoplankton_Colour_Index)) %>%
  pr_apply_Time() %>% #TODO added for consistency but uses etc timezones - do we changes these to the more familiar names or leave? doesn't improve with method = accurate
  planktonr_dat(Survey = "CPR", Type = "Zooplankton") %>%
  pr_add_Bioregions(near_dist_km = 250) %>% # 250km buffer
  select(-c("Time", "Date", "Month", "Year", "Season", "Tow_Number", "Segment_No.",
            "Fluorescence", "Salinity", "Water_Temperature", "Ship_Code", "Segment_Length",
            "Photosynthetically_Active_Radiation", "cellID", "Colour")) %>% # Remove original Time and Date columns
  dplyr::select(tidyselect::all_of(c("SampleTime_Local", "Year_Local", "Month_Local",
                                     "SampleTime_UTC", "tz", "TripCode", "SampleVolume_m3",
                                      "BioRegion", "Latitude", "Longitude",
                                     "Sample_ID", "TotalCount" = "Total abundance",
                                     "PCI" = "Phytoplankton_Colour_Index")),
                tidyselect::everything())

SpInfoZ <- planktonr::pr_get_SpeciesInfo(Type = "Zooplankton") %>%
  dplyr::mutate(`Taxon Name` = stringr::str_remove(`Taxon Name`, " [fmji]$"),
                `Taxon Name` = stringr::str_remove(`Taxon Name`," megalopa"),
                `Taxon Name` = stringr::str_remove(`Taxon Name`," naupliius"),
                `Taxon Name` = stringr::str_remove(`Taxon Name`," phyllosoma"),
                `Taxon Name` = stringr::str_remove(`Taxon Name`," zoea")) %>%
  distinct(`Taxon Name`, .keep_all = TRUE)


AAD_cols <- c("SampleTime_Local", "Year_Local", "Month_Local", "SampleTime_UTC",
             "tz", "TripCode", "SampleVolume_m3",
              "BioRegion", "Latitude", "Longitude",
              "Sample_ID", "TotalCount", "PCI")

spp <- cpr_AAD %>%
  select(-tidyselect::all_of(AAD_cols)) %>%
  colnames() %>%
  stringr::str_remove(" \\(.*?\\)") %>%
  stringr::str_remove(" [CF][123456]$") %>%
  stringr::str_replace_all(" sp.$", " spp.") %>%
  stringr::str_remove_all(" indet") %>%
  stringr::str_replace_all("oda$", "od") %>%
  stringr::str_replace_all("oda ", "od ") %>%
  stringr::str_replace_all("coida$", "coid") %>%
  stringr::str_replace_all("nauplius", "nauplii") %>%
  stringr::str_remove_all(" megalopa") %>%
  stringr::str_remove_all(" nauplii") %>%
  stringr::str_remove_all(" phyllosoma") %>%
  stringr::str_remove_all(" zoea") %>%
  stringr::str_remove_all(" larvae") %>%
  stringr::str_remove_all(" juv$") %>%
  stringr::str_remove_all(" cyprid") %>%
  stringr::str_replace_all(" var ", " var. ") %>%
  stringr::str_replace_all("Ctenophora", "Ctenophore") %>%
  stringr::str_replace_all("Branchiopod", "Branchiopoda") %>%
  stringr::str_replace_all("Bivalvia", "Bivalve") %>%
  stringr::str_replace_all("Bryozoa", "Bryozoan (Cyphonaute larvae)") %>%
  stringr::str_replace_all("Cirripedia", "Cyprididae") %>%
  stringr::str_replace_all("Clione limacina antarctica", "Clione antarctica") %>%
  stringr::str_replace_all("Cnidaria", "Medusa") %>%
  stringr::str_replace_all("Cyclopoida", "Poecilostomatoida / Cyclopoid") %>%
  stringr::str_replace_all("Echinoidea", "Echinoderm larvae") %>%
  stringr::str_replace_all("Euphausia similis var armata", "Euphausia similis var armata") %>%
  stringr::str_replace_all("Euphausiidae calyptopis", "Euphausiid calyptope") %>%
  stringr::str_replace_all("Euphausiidae furcilia", "Euphausiid furcilia larvae") %>%
  stringr::str_replace_all("Euphausiidae", "Euphausiid adult") %>%
  stringr::str_replace_all("Euphausiid adult metanauplii", "Euphausiidae metanauplii") %>%
  stringr::str_replace_all("Fritillaria spp.", "Fritillariidae") %>%
  stringr::str_replace_all("Gammaridea", "Gammarid amphipod") %>%
  stringr::str_replace_all("Hemityphis spp.", "Hemityphis spp.") %>%
  stringr::str_replace_all("Hyperiidea", "Hyperiid amphipod") %>%
  stringr::str_replace_all("Limacina spp.", "Limacina spp.") %>%
  stringr::str_replace_all("Myctophidae", "Myctophid") %>%
  stringr::str_replace_all("Mysidae", "Mysid") %>%
  stringr::str_replace_all("Nauplius", "Nauplii zooplankton") %>%
  stringr::str_replace_all("Pisces", "Pisces larvae") %>%
  stringr::str_replace_all("Polychaeta", "Polychaete") %>%
  stringr::str_replace_all("Siphonophorae nectophore", "Siphonophore / nectophore") %>%
  stringr::str_replace_all("Siphonophorae spp.", "Siphonophore") %>%
  stringr::str_replace_all("Thysanoessa gregaria calyptopis", "Thysanoessa gregaria calyptope") %>%
  stringr::str_replace_all("Thysanoessa macrura metanauplii", "Thysanoessa macrura metanauplius") %>%
  stringr::str_replace_all("Thysanoessa sp. furcilia", "Thysanoessa spp. furcilia") %>%
  stringr::str_replace_all("Pisces larvae egg", "Fish egg")


colnames(cpr_AAD[(length(AAD_cols)+1):length(colnames(cpr_AAD))]) <- spp # Rename columns with correct names


# Check NAs
# spp <- tibble(Name = spp)
#
# spp2 <- spp %>%
#   left_join(SpInfoZ, by = c("Name" = "Taxon Name")) %>%
#   group_split(by = is.na(`WoRMS AphiaID`))
#
# sum(is.na(spp2[[2]]$`WoRMS AphiaID`))
# sppNA <- spp2[[2]]


# Coastal Data ------------------------------------------------------------

# Details for coastal stations
CSCodes <- tibble::tibble(StationName = c("Balls Head", "Salmon Haul", "Bare Island - Botany Bay", "Cobblers Beach", "Towra Point - Botany Bay",
                                          "Lilli Pilli","Derwent Estuary B1", "Derwent Estuary B3", "Derwent Estuary E", "Derwent Estuary G2",
                                          "Derwent Estuary KB", "Derwent Estuary RBN", "Derwent Estuary U2", "Low Head",
                                          "Tully River Mouth mooring", "Russell-Mulgrave River mooring", "Green Island", "Port Douglas",
                                          "Cape Tribulation", "Double Island", "Yorkey's Knob", "Fairlead Buoy", "Hobsons - Port Phillip Bay",
                                          "Long Reef - Port Phillip Bay", "Geoffrey Bay", "Channel", "Pioneer Bay", "Centaur Reef",
                                          "Wreck Rock", "Inshore reef_Channel", "Inshore reef_Geoffrey Bay"),
                          StationCode = c("BAH", "SAH", "BAI", "COB", "TOP", "LIP", "DEB", "DES", "DEE", "DEG", "DEK", "DER", "DEU", "LOH",
                                          "TRM", "RMR", "GNI", "PTD", "CTL", "DBI", "YKK", "FLB", "HOB", "LOR", "GEB", "CHA", "PIB", "CER",
                                          "WRR", "IRC", "IGB"),
                          State = factor(c("NSW", "NSW", "NSW", "NSW", "NSW", "NSW", "TAS", "TAS","TAS","TAS","TAS","TAS","TAS", "TAS", "GBR",
                                           "GBR","GBR","GBR","GBR","GBR","GBR","GBR", "VIC", "VIC","GBR","GBR","GBR","WA", "WA","GBR","GBR" ),
                                         levels = c("GBR", "NSW", "WA", "VIC", "TAS"))) %>%
  dplyr::arrange(State)



# Microbial Coastal station input into pl_plot_NRSmap()
csDAT <- planktonr::pr_get_NRSMicro("Coastal") %>%
  dplyr::select("StationName", "StationCode", "Longitude", "Latitude", "State") %>%
  # dplyr::rename(Code = "StationCode",
  # Station = "StationName") %>%
  dplyr::group_by(StationCode, StationName, State) %>%
  dplyr::summarise(Latitude = mean(Latitude, na.rm = TRUE),
                   Longitude = mean(Longitude, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::distinct() %>%
  dplyr::arrange(desc(State), desc(Latitude)) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)



## Coastal station colours
stateCol <- c(rep("#8FE388", 5), rep("#1B998B", 4), rep("#CBFF8C", 4), rep("#FFBA08",6), rep("#3185FC",2), rep("#5D2E8C",2), rep("#FF7B9C",4), rep("#FF9B85", 4))
stateLTY <- c("solid", "dashed", "dotted", "dotdash", "longdash", "solid", "dashed", "dotted", "dotdash", "solid", "dashed", "dotted", "dotdash",
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



# National Reference Station ----------------------------------------------


# NRS input into pl_plot_NRSmap()
meta_sf <- planktonr::pr_get_NRSTrips() %>%
  dplyr::select("StationName", "StationCode", "Longitude", "Latitude") %>%
  dplyr::distinct() %>%
  dplyr::rename(Code = "StationCode",
                Station = "StationName") %>%
  dplyr::filter(Station != 'Port Hacking 4') %>%
  dplyr::arrange(desc(Latitude)) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)



# https://coolors.co/palette/d00000-ffba08-cbff8c-8fe388-1b998b-3185fc-5d2e8c-46237a-ff7b9c-ff9b85
# Darwin Yongala Ningaloo North Stradbroke Island Rottnest Island Esperance Port Hacking Kangaroo Island Maria Island
# "#ff8500" "#b66ee8" "#ff9b85" "#d00000". "#46237a" "#1b998b" "#8fe388" "#ff7b9c" "#3185fc"


coolor <- c("#ff8500", "#b66ee8", "#ff9b85", "#d00000", "#46237a", "#1b998b", "#8fe388", "#ff7b9c", "#3185fc", "#391d26") #"#cbff8c" "#ffba08"

colNRSCode <- data.frame(Code = meta_sf$Code,
                         Colr = coolor) %>%
  tibble::deframe()

colNRSName <- data.frame(Code = meta_sf$Station,
                         Colr = coolor) %>%
  tibble::deframe()

pchNRSName <- data.frame(Code = meta_sf$Station,
                         pchr = rep(16, 10)) %>%
  tibble::deframe()

pchNRSCode <- data.frame(Code = meta_sf$Code,
                         pchr = rep(16, 10)) %>%
  tibble::deframe()

ltyNRSCode <- data.frame(Code = meta_sf$Code,
                         pchr = rep("solid", 10)) %>%
  tibble::deframe()

ltyNRSName <- data.frame(Code = meta_sf$Station,
                         pchr = rep("solid", 10)) %>%
  tibble::deframe()


colNRSCode <- c(colNRSCode, colCSCode)
colNRSName <- c(colNRSName, colCSName)
pchNRSCode <- c(pchNRSCode, pchCSCode)
pchNRSName <- c(pchNRSName, pchCSName)
ltyNRSCode <- c(ltyNRSCode, ltyCSCode)
ltyNRSName <- c(ltyNRSName, ltyCSName)




rm(colCSCode, colCSName, pchCSCode, pchCSName, ltyCSCode, ltyCSName, stateCol, stateLTY, statePCH)



usethis::use_data(mbr, MapOz, meta_sf, csDAT, cpr_AAD,
                  colCPR, pchCPR, ltyCPR, CPRinfo, CSCodes,
                  colNRSCode, colNRSName, pchNRSName, pchNRSCode, ltyNRSCode, ltyNRSName,
                  overwrite = TRUE, internal = TRUE, compress = "bzip2")

usethis::use_data(mbr, csDAT, overwrite = TRUE, internal = FALSE, compress = "bzip2")

# tools::checkRdaFiles("R") # Check what compression to use above
# OK - bzip2
