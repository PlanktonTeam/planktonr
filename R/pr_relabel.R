
#' Relabel string for display
#'
#' @param s A string of names to be relabeled.
#'
#' @return Dataframe with new column names
pr_relabel <- function(s){

  relabel_df <- tibble::as_tibble(matrix(c(
    "TotalChla", expr(paste("Total Chlorophyll ",italic(a)," (mg m"^-3,")")),
    "PPC", expr(paste("Photoprotective Carotenoids (mg m"^-3,")")),
    "PSC", expr(paste("Photosynthetic Carotenoids (mg m"^-3,")")),
    "PSP", expr(paste("Photosynthetic Pigments (mg m"^-3,")")),
    "TCaro", expr(paste("Total Carotenoids (mg m"^-3,")")),
    "TAcc", expr(paste("Total Accessory Pigments (mg m"^-3,")")),
    "TPig", expr(paste("Total Pigments (mg m"^-3,")")),
    "TDP", expr(paste("Total Diagnostic pigments (mg m"^-3,")")),
    "Ammonium_umolL", expr(paste("Ammonium (",mu,"mol L"^-1,")")),
    "Biomass_mgm3", expr(paste("Biomass (mg m"^-3,")")),
    "Biovolume_um3L", expr(paste("Biovolume (mum"^3," L"^-1,")")),
    "BioVolume_um3m3", expr(paste("Biovolume (mum"^3," m"^-3,")")),
    "Cells_L", expr(paste("Cells (L"^-1,")")),
    "Chla_mgm3", expr(paste("Chla (mg m"^-3,")")),
    "Conductivity_Sm", expr(paste("Conductivity (Sm")),
    "Density_kgm3", expr(paste("Density (kg m"^-3,")")),
    "DIC_umolkg", expr(paste("DIC (",mu,"mol kg"^-1,")")),
    "DissolvedOxygen_umolkg", expr(paste("Dissolved Oxygen (",mu,"mol kg"^-1,")")),
    "GearDepth_m", expr(paste("GearDepth (m)")),
    "GearMesh_um", expr(paste("GearMesh (",mu,"m")),
    "InorganicFraction_mgL", expr(paste("Inorganic Fraction (mg L"^-1,")")),
    "Length_mm", expr(paste("Length (mm)")),
    "Nitrate_umolL", expr(paste("Nitrate (",mu,"mol L"^-1,")")),
    "Nitrite_umolL", expr(paste("Nitrite (",mu,"mol L"^-1,")")),
    "OrganicFraction_mgL", expr(paste("Organic Fraction (mg L"^-1,")")),
    "Oxygen_umolL", expr(paste("Oxygen (",mu,"mol L"^-1,")")),
    "Phosphate_umolL", expr(paste("Phosphate (",mu,"mol L"^-1,")")),
    "PhytoAbund_m3", expr(paste("Phytoplankton Abundance (m"^-3,")")),
    "Picoeukaryotes_Cellsml", expr(paste("Picoeukaryotes (cells ml"^-1,")")),
    "Pressure_dbar", expr(paste("Pressure (dbar)")),
    "Prochlorococcus_Cellsml", expr(paste("Prochlorococcus (cells ml"^-1,")")),
    "Salinity_psu", expr(paste("Salinity (psu)")),
    "SampleDateLocal", expr(paste("Sample Date (Local)")),
    "SampleDateUTC", expr(paste("Sample Date (UTC)")),
    "SampleDepth_m", expr(paste("Sample Depth (m)")),
    "SampVol_L", expr(paste("Sample Vol. (L)")),
    "SampVol_m3", expr(paste("Sample Vol. (m"^-3,")")),
    "Secchi_m", expr(paste("Secchi Depth (m)")),
    "Silicate_umolL", expr(paste("Silicate (",mu,"mol L"^-1,")")),
    "StationCode", expr(paste("Station Code")),
    "StationName", expr(paste("Station Name")),
    "Synecochoccus_Cellsml", expr(paste("Synecochoccus (cells ml"^-1,")")),
    "TAlkalinity_umolkg", expr(paste("TAlkalinity (",mu,"mol kg"^-1,")")),
    "TaxonCount", expr(paste("Taxon Count")),
    "TaxonGroup", expr(paste("Taxon Group")),
    "TaxonName", expr(paste("Taxon Name")),
    "Temperature_degC", expr(paste("Temperature (",degree,"C)")),
    "TowType", expr(paste("Tow Type")),
    "TripCode", expr(paste("Trip Code")),
    "TSS_mgL", expr(paste("TSS (mg L"^-1,")")),
    "Turbidity_NTU", expr(paste("Turbidity (NTU)")),
    "Volume_m3", expr(paste("Volume m"^-3,")")),
    "WaterDensity_kgm3", expr(paste("Water Density (kg m"^-3,")")),
    "WaterDepth_m", expr(paste("Water Depth (m)")),
    "ZoopAbund_m3", expr(paste("Zooplankton Abundance (m"^-3,")"))),
    ncol = 2, byrow = TRUE, dimnames = list(NULL, c("St","Ex"))))

  i <- which(str_detect(relabel_df$St, s))

  return(relabel_df$Ex[[i]])
}
