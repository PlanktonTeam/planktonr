
# satcpr <- cprSampleInfo %>%
# dplyr::rename(Date = SampleDateUTC)

# if (get_sat_data == TRUE){
#   # accessing the satelitte data from MODIS
#
#   # Possible products
#   # pr <- c("sst_quality", "sst", "picop_brewin2012in", "picop_brewin2010at", "par",
#   #         "owtd", "npp_vgpm_eppley_oc3", "npp_vgpm_eppley_gsm", "nanop_brewin2012in",
#   #         "nanop_brewin2010at", "l2_flags", "ipar", "dt", "chl_oc3", "chl_gsm", "K_490")
#
#   pr <- c("sst", "chl_oc3")
#   res_temp <- "1d"
#   res_spat <- 10 # Return the average of res_spat x res_spat pixels
#
#   # Get MODIS Data
#   satcpr <- fIMOS_MatchMODIS(satcpr, pr, res_temp, res_spat)
#
#   # Get Altimetry (Gridded sea level anomaly, Gridded sea level, Surface geostrophic velocity)
#   satcpr <- fIMOS_MatchAltimetry(satcpr, res_spat)
# } else{
#   satcpr <- satcpr %>%
#     add_column(sst_1d = NA, chl_oc3_1d = NA)
# }
