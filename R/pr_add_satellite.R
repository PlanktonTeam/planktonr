
# satcpr <- cprSampleInfo %>%

# rename(.data$Date = .data$SampleDateUTC)

# if (pr_get_sat_data == TRUE){
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





# # Access satellite data for the sample dates using the IMOS_Toolbox
#
# # If on Windows you will need to install a development
# # version of ncdf4 which allows the use of OpenDAP
# if(.Platform$OS.type == "windows") {
#  warning("It looks like you are on a Windows PC - You will need to install a
#  development version of ncdf4 which allows the use of OpenDAP. Please
#  run devtools::install_github("mdsumner/ncdf4") to install or
#  see "https://github.com/mdsumner/ncdf4" for more information.")
# }
#
# # Get GHRSST SST Data
# # Possible products to download are:
# # dt_analysis, l2p_flags, quality_level, satellite_zenith_angle, sea_ice_fraction, sea_ice_fraction_dtime_from_sst,
# # sea_surface_temperature, sea_surface_temperature_day_night, sses_bias, sses_count,sses_standard_deviation,
# # sst_count, sst_dtime, sst_mean, sst_standard_deviation, wind_speed, wind_speed_dtime_from_sst,
# res_temp <- "1d"
# res_spat <- 10 # Return the average of res_spat x res_spat pixels
# pr <- ("sea_surface_temperature")
# GHRSST <- fIMOS_MatchGHRSST(dNRSdat, pr, res_temp, res_spat)
#
# # Get MODIS Data
# # Possible products
# # pr <- c("sst_quality", "sst", "picop_brewin2012in", "picop_brewin2010at", "par",
# #     "owtd", "npp_vgpm_eppley_oc3", "npp_vgpm_eppley_gsm", "nanop_brewin2012in",
# #     "nanop_brewin2010at", "l2_flags", "ipar", "dt", "chl_oc3", "chl_gsm", "K_490")
#
# pr <- c("chl_oci")
# res_temp <- "1d"
# res_spat <- 10 # Return the average of res_spat x res_spat pixels
# MODIS <- fIMOS_MatchMODIS(dNRSdat, pr, res_temp, res_spat)
#
#
# # Get Altimetry (Gridded sea level anomaly, Gridded sea level, Surface geostrophic velocity)
# dNRSdat <- dNRSdat[1:3,]
# Alt <- fIMOS_MatchAltimetry(dNRSdat, res_spat)




# %>%
#  left_join(GHRSST %>% select(-c("Longitude", "Latitude", "Date")), by = ("TripCode")) %>%
#  left_join(MODIS %>% select(-c("Longitude", "Latitude", "Date")), by = ("TripCode")) %>%
#  left_join(Alt %>% select(c("TripCode", "GSLA", "GSL", "UCUR", "VCUR")), by = ("TripCode"))
