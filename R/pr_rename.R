
#' Standardise column names in IMOS plankton dataframes
#'
#' @param dat Dataframe with column names to renamed
#'
#' @return Dataframe with new column names
#' @keywords internal
pr_rename <- function(dat){

  ##TODO - Check and remvoe any uneeded renames - note the capitals are needed for files grabbed from S3
  rename_df <- tibble::as_tibble(matrix(c(
    "Sample", "SAMPLE",
    "Region", "REGION",
    "AshFreeBiomass_mgm3", "ASHFREEBIOMASS_MGM3",
    "Biomass_mgm3", "BIOMASS_MGM3",
    "Cell BioVolume (um3)", "BV_CELL",
    "Cell Carbon (pgN cell-1)", "PGC_CELL",
    "Cell Nitrogen (pgN cell-1)", "MONT2001PGN_CELL",
    "Diet", "DIET",
    "Functional Type", "TROPHY",
    "Functional Group", "FUNCTIONALGROUP",
    "Genus", "GENUS",
    "Latitude", "LATITUDE",
    "Longitude", "LONGITUDE",
    "Length (mm)", "LENGTH_MM",
    "Methods", "METHODS",
    "Nitrate_umolL", "NITRATE_VALUE",
    "Nitrate_Flag", "NITRATE_QC_FLAG",
    "Nitrite_umolL", "NITRITE_VALUE",
    "Nitrite_Flag", "NITRITE_QC_FLAG",
    "Oxygen_umolL", "OXYGEN_VALUE",
    "Oxygen_Flag", "OXYGEN_QC_FLAG",
    "Phosphate_umolL", "PHOSPHATE_VALUE",
    "Phosphate_Flag", "PHOSPHATE_QC_FLAG",
    "Picoeukaryotes_cellsmL", "Picoeukaryotes_cells_ml",
    "Prochlorococcus_cellsmL", "Prochlorococcus_cells_ml",
    "Reference (Size)", "SIZEREFERENCE",
    "SampleTime_UTC", "SAMPLEDATEUTC",  # put these back as they come from S3 pr_get_CPRtrips()
    "SampleTime_Local", "SAMPLEDATELOCAL",
    "Salinity", "SALINITY_VALUE",
    "Salinity_Flag", "SALINITY_QC_FLAG",
    "Silicate_umolL", "SILICATE_VALUE",
    "Silicate_Flag", "SILICATE_QC_FLAG",
    "Species", "SPECIES",
    "Synechococcus_cellsmL", "Synechococcus_cells_ml",
    "Temperature_degC", "TEMPERATURE_VALUE",
    "Temperature_Flag", "TEMPERATURE_QC_FLAG",
    "Ammonium_umolL", "AMMONIA_VALUE",
    "Ammonium_Flag", "AMMONIA_QC_FLAG",
    "ProjectName", "PROJECTNAME",
    "PSampleDepth_m", "PHYTOSAMPLEDEPTH_M",
    "SampleType", "SAMPLETYPE",
    "Secchi_m", "SECCHI_M",
    "StationName", "STATIONNAME",
    "StationCode", "STATIONCODE",
    "Sub Genera", "SUBGENERA",
    "Taxon Name", "TAXON_NAME",
    "TripCode", "TRIP_CODE",
    "WoRMS AphiaID", "SPCODE",
    "ZSampleDepth_m", "ZOOPSAMPLEDEPTH_M"),
    ncol = 2, byrow = TRUE, dimnames = list(NULL, c("New", "Old"))))

  dat <- data.table::setnames(dat, old = rename_df$Old, new = rename_df$New, skip_absent = TRUE)

  return(dat)
}
