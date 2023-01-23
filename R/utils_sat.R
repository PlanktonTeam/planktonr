#' Functions for matching location data to satellite products

#' Get data for satellite matching
#'
#' @param Survey NRS, CPR or all
#'
#' @return df with latitude, longitude and Date
#' @export
#'
#' @examples
#' df <- pr_get_DataLocs("NRS")

pr_get_DataLocs <- function(Survey = 'all'){

  if(Survey == 'NRS'){
    df <- pr_get_NRSTrips(c('P', 'Z')) %>%
      dplyr::select("Longitude", "Latitude", "SampleTime_UTC") %>%
      dplyr::distinct(.data$Longitude, .data$Latitude, Date = as.Date(.data$SampleTime_UTC, 'UTC'))
  } else
  if(Survey == 'CPR'){
    df <- pr_get_CPRTrips() %>%
      dplyr::filter(grepl("P|Z", .data$SampleType)) %>%
      dplyr::select("Longitude", "Latitude", "SampleTime_UTC") %>%
      dplyr::distinct(.data$Longitude, .data$Latitude, Date = as.Date(.data$SampleTime_UTC, 'UTC'))
  } else {
    df <- dplyr::bind_rows(
      pr_get_NRSTrips(c('P', 'Z')) %>%
        dplyr::select("Longitude", "Latitude", "SampleTime_UTC"),
      pr_get_CPRTrips() %>%
        dplyr::filter(grepl("P|Z", .data$SampleType)) %>%
        dplyr::select("Longitude", "Latitude", "SampleTime_UTC")) %>%
      dplyr::distinct(.data$Longitude, .data$Latitude, Date = as.Date(.data$SampleTime_UTC, 'UTC'))
  }

}

#' #' Title
#' #'
#' #' @param df
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' pr_check_DataLocs <- function(df){
#'   #TODO remove locations that already have data associated with them
#' }


#' Match locations to GHRSST
#' Optional Inputs:
#' res_spat - Spatial resolution. How many pixels (n x n) to download in each direction
#' res_temp - What temporal averaging: 1 day (1d), 6 day (6d), 1 month(1m), ,...
#' Monthly climatology (1mNy), Annual climatology (12mNy)

#' Possible products to download are:
#' dt_analysis, l2p_flags, quality_level, satellite_zenith_angle, sea_ice_fraction, sea_ice_fraction_dtime_from_sst,
#' sea_surface_temperature, sea_surface_temperature_day_night, sses_bias, sses_count,sses_standard_deviation,
#' sst_count, sst_dtime, sst_mean, sst_standard_deviation, wind_speed, wind_speed_dtime_from_sst
#'
#' @param df dataframe containing latitude, longitude and Date
#' @param pr products from list above, single or as a list
#'
#' @return df with product output attached
#' @export
#'
#' @examples
#' df <- head(pr_get_DataLocs("NRS"),5)
#' res_spat = 10
#' res_temp = "6d"
#' pr = c("sea_surface_temperature", "sea_surface_temperature_day_night")
#' sstout <- pr_match_GHRSST(df, pr)
#' #TODO add progress bars with purrr

pr_match_GHRSST <- function(df, pr) {

  # Set resolution
  if (!exists("res_temp")){
    print("Defaulting to daily satellite data. Provide res_temp if you want to change")
    res_temp <-  "1d"
  }

  if (!exists("res_spat")){
    print("Defaulting to 1 pixel x 1 pixel. Provide res_spat if you want to increase")
    res_spat <-  1
  }

  if (("Date" %in% colnames(df))==FALSE) {
    stop("No Date column found in data. Please include a Date column in POSIXct format")
  }

  # If Day, Month, Year doesn't exist we create them
  if (sum(c("Day","Month","Year") %in% colnames(df)) != 3) {
    df <- df %>%
      dplyr::mutate(Day = lubridate::day(.data$Date),
             Month = lubridate::month(.data$Date),
             Year = lubridate::year(.data$Date))
  }

  df <- df %>%
    dplyr::select("Latitude", "Longitude", "Year", "Month", "Day")

  df <- dplyr::bind_cols(purrr::map_dfr(seq_len(length(pr)), ~ df),
                         purrr::map_dfr(seq_len(nrow(df)), ~ data.frame(pr)) %>%
                           dplyr::arrange(pr)) %>%
    dplyr::group_split(.data$Latitude, .data$Longitude, .data$Year, .data$Month, .data$Day, .data$pr)

  pr_get_SSTData <- function(df){

    # Make sure month and day have a leading zero if less than 10
    mth <- stringr::str_pad(df$Month,2,"left",pad="0")
    dy <- stringr::str_pad(df$Day,2,"left",pad="0")

    if(res_temp == '6d'){
      string = "212000"
    } else {
      string = "092000"
    }

    url_base <- paste0("http://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/SST/ghrsst/L3S-",res_temp,"/dn/") # Base URL
    imos_url <- paste0(url_base, df$Year,"/",df$Year,mth,dy,string,"-ABOM-L3S_GHRSST-SSTfnd-AVHRR_D-", res_temp, "_dn.nc")

    tryCatch({ # Not all dates will exist

      nc <- RNetCDF::open.nc(imos_url)
      lat <- RNetCDF::var.get.nc(nc, variable = "lat")
      lon <- RNetCDF::var.get.nc(nc, variable = "lon")
      lengthlat <- RNetCDF::dim.inq.nc(nc, "lat")
      lengthlon <- RNetCDF::dim.inq.nc(nc, "lon")
      minlat <- min(lat)
      maxlat <- max(lat)
      minlon <- min(lon)
      maxlon <- max(lon)

      # Approximate nearest neighbour
      idx_lon <- yaImpute::ann(as.matrix(seq(minlon, maxlon, length.out = lengthlon$length)), as.matrix(df$Longitude), k = 1, verbose = FALSE)$knnIndexDist[,1]
      idx_lat <- yaImpute::ann(as.matrix(seq(maxlat, minlat, length.out = lengthlat$length)), as.matrix(df$Latitude), k = 1, verbose = FALSE)$knnIndexDist[,1]
      cnt <- c(1,1,1)

      if (res_spat > 1) { # If more than 1x1 pixel is requested we adjust the idx by res_spat/2 and count by res_spa
        idx_lon <- idx_lon - floor(res_spat/2)
        idx_lat <- idx_lat - floor(res_spat/2)
        cnt <- c(res_spat, res_spat, 1)
      }
      out <- RNetCDF::var.get.nc(nc, df$pr, start=c(idx_lon, idx_lat, 1), count = cnt, unpack = TRUE)

      out <- mean(out, na.rm = TRUE) - 273.15
      return(out)
      RNetCDF::close.nc(nc)
    },
    error = function(cond) {
      out <- NaN
      return(out)
    }
    )

  }
  sstout <- purrr::map(df, pr_get_SSTData)
  df <- dplyr::bind_rows(df) %>%
    dplyr::bind_cols(value = unlist(sstout)) %>%
    tidyr::pivot_wider(names_from = "pr", values_from = "value")

}


#' Match data for altimetry
#'
#' @param df dataframe containing Latitude, Longitude and Date
#' @param pr products from GSLA, GSL, UCUR, UCUR, VCUR, UCUR_MEAN, VCUR_MEAN, single or as a list
#'
#' @return df with product output attached
#' @export
#'
#' @examples
#' df <- tail(pr_get_DataLocs("NRS"), 5)
#' res_spat <- 10
#' altout <- pr_match_Altimetry(df, pr = "GSLA")
pr_match_Altimetry <- function(df, pr) {

  if (!exists("res_spat")){
    print("Defaulting to 1 pixel x 1 pixel. Provide res_spat if you want to increase")
    res_spat <-  1
  }

  if (sum(c("Day","Month","Year") %in% colnames(df)) != 3) { # Check that Day, Month, Year exists
    if (sum(stringr::str_detect(colnames(df),"Date")) == 1) { # Otherwise check that Date exists
      df <- df %>%
        dplyr::mutate(Day = lubridate::day(.data$Date),
               Month = lubridate::month(.data$Date),
               Year = lubridate::year(.data$Date))
    } else {
      print("Missing Date or Day/Month/Year columns")
    }
  }

  if (max(df$Year > 2019)){
    print("Only data before 2020 is available via this product and will be returned from this function")
  }

  df <- df %>%
    dplyr::select("Latitude", "Longitude", "Year", "Month", "Day")

  df <- dplyr::bind_cols(purrr::map_dfr(seq_len(length(pr)), ~ df),
                         purrr::map_dfr(seq_len(nrow(df)), ~ data.frame(pr)) %>%
                           dplyr::arrange(pr)) %>%
    dplyr::group_split(.data$Latitude, .data$Longitude, .data$Year, .data$Month, .data$Day, .data$pr)


  pr_get_SatData <- function(df){
    # Make sure month and day have a leading zero if less than 10
    mth <- stringr::str_pad(df$Month,2,"left",pad="0")
    dy <- stringr::str_pad(df$Day,2,"left",pad="0")

    tryCatch({
      thredd_url <- paste0("http://thredds.aodn.org.au/thredds/catalog/IMOS/OceanCurrent/GSLA/DM01/",df$Year,"/catalog.xml")
      f <- thredds::CatalogNode$new(thredd_url)
      files <- data.frame(files = f$get_dataset_names())
      pattern <- paste0(df$Year,mth,dy,"T000000Z")
      fileName <- files %>% dplyr::filter(grepl(pattern, files))
      filename <- fileName$files
      url_base <- paste0("http://thredds.aodn.org.au/thredds/dodsC/IMOS/OceanCurrent/GSLA/DM01/") # Base URL
      imos_url <- paste0(url_base,df$Year,"/",filename)
      },
      error = function(cond) {
        x <- NA
        return(x)
        }
    )

    if(exists("fileName") && nrow(fileName) > 0) {# Not all dates will exist
      nc <- RNetCDF::open.nc(imos_url)
      lat <- RNetCDF::var.get.nc(nc, variable = "LATITUDE")
      lon <- RNetCDF::var.get.nc(nc, variable = "LONGITUDE")
      lengthlat <- RNetCDF::dim.inq.nc(nc, "LATITUDE")
      lengthlon <- RNetCDF::dim.inq.nc(nc, "LONGITUDE")
      minlat <- min(lat)
      maxlat <- max(lat)
      minlon <- min(lon)
      maxlon <- max(lon)

      # Approximate nearest neighbour
      idx_lon <- (yaImpute::ann(as.matrix(seq(minlon, maxlon, length.out = lengthlon$length)), as.matrix(df$Longitude), k = 1, verbose = FALSE)$knnIndexDist[,1])[1]
      idx_lat <- (yaImpute::ann(as.matrix(seq(maxlat, minlat, length.out = lengthlat$length)), as.matrix(df$Latitude), k = 1, verbose = FALSE)$knnIndexDist[,1])[1]
      cnt <- c(1,1,1)

      if (res_spat > 1) { # If more than 1x1 pixel is requested we adjust the idx by res_spat/2 and count by res_spa
        idx_lon <- idx_lon - floor(res_spat/2)
        idx_lat <- idx_lat - floor(res_spat/2)
        cnt <- c(res_spat, res_spat, 1)
      }

      out <- RNetCDF::var.get.nc(nc, df$pr, start=c(idx_lon, idx_lat, 1), count = cnt, unpack = TRUE)

      out <- mean(out, na.rm = TRUE)
      RNetCDF::close.nc(nc)
    } else {
      out <- NA
    }
    return(out)
  }

  altout <- purrr::map(df, pr_get_SatData)

  df <- dplyr::bind_rows(df) %>%
    dplyr::bind_cols(value = unlist(altout)) %>%
    tidyr::pivot_wider(names_from = "pr", values_from = "value")
}

#' Match data for MODIS
#'
#' @param df dataframe containing latitude, longitude and Date
#' @param pr products from K_490, chl_carder, chl_gsm, chl_oc3, chl_oci, dt, ipar, l2_flags, owtd, par, sst, sst_quality single or as a list
#'
#' @return df with product output attached
#' @export
#'
#' @examples
#' df <- head(pr_get_DataLocs("NRS"),5)
#' res_spat = 10
#' MODISout <- pr_match_MODIS(df, pr <- c("chl_gsm", "chl_oc3"))
pr_match_MODIS <- function(df, pr) {

  # Set resolution
  if (!exists("res_temp")){
    print("Defaulting to daily satellite data")
    res_temp <-  "1d"
  }

  if (!exists("res_spat")){
    print("Defaulting to 1 pixel x 1 pixel. Provide res_spat if you want to increase")
    res_spat <-  1
  }

  if (("Date" %in% colnames(df))==FALSE) {
    stop("No Date column found in data. Please include a Date column in POSIXct format")
  }

  # If Day, Month, Year doesn't exist we create them
  if (sum(c("Day","Month","Year") %in% colnames(df)) != 3) {
    df <- df %>%
      dplyr::mutate(Day = lubridate::day(.data$Date),
             Month = lubridate::month(.data$Date),
             Year = lubridate::year(.data$Date))
  }

  if (min(df$Date) < as.Date("2002-07-01")){
    print("Only data after 2002-07-01 is available via this product and will be returned from this function")
  }

  df <- df  %>%
    dplyr::select("Latitude", "Longitude", "Year", "Month", "Day")

  df <- dplyr::bind_cols(purrr::map_dfr(seq_len(length(pr)), ~ df),
                         purrr::map_dfr(seq_len(nrow(df)), ~ data.frame(pr)) %>%
                           dplyr::arrange(pr)) %>%
    dplyr::group_split(.data$Latitude, .data$Longitude, .data$Year, .data$Month, .data$Day, .data$pr)

  pr_get_SatData <- function(df){
    # Make sure month and day have a leading zero if less than 10
    mth <- stringr::str_pad(df$Month,2,"left",pad="0")
    dy <- stringr::str_pad(df$Day,2,"left",pad="0")

    tryCatch({ # Not all dates will exist
      url_base <- paste0("http://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/OC/gridded/aqua/P1D/") # Base URL
      imos_url <- paste0(url_base, df$Year,"/",mth,"/A.P1D.",df$Year,mth,dy,"T053000Z.aust.",df$pr,".nc")
      nc <- RNetCDF::open.nc(imos_url)
      },
      error = function(cond) {
        ncx <- NaN
        return(ncx)
        }
    )

    if(exists("nc")) {# Not all dates will exist

      lat <- RNetCDF::var.get.nc(nc, variable = "latitude")
      lon <- RNetCDF::var.get.nc(nc, variable = "longitude")
      lengthlat <- RNetCDF::dim.inq.nc(nc, "latitude")
      lengthlon <- RNetCDF::dim.inq.nc(nc, "longitude")
      minlat <- min(lat)
      maxlat <- max(lat)
      minlon <- min(lon)
      maxlon <- max(lon)

      # Approximate nearest neighbour
      idx_lon <- yaImpute::ann(as.matrix(seq(minlon, maxlon, length.out = lengthlon$length)), as.matrix(df$Longitude), k = 1, verbose = FALSE)$knnIndexDist[,1]
      idx_lat <- yaImpute::ann(as.matrix(seq(maxlat, minlat, length.out = lengthlat$length)), as.matrix(df$Latitude), k = 1, verbose = FALSE)$knnIndexDist[,1]
      cnt <- c(1,1,1)

      if (res_spat > 1) { # If more than 1x1 pixel is requested we adjust the idx by res_spat/2 and count by res_spa
        idx_lon <- idx_lon - floor(res_spat/2)
        idx_lat <- idx_lat - floor(res_spat/2)
        cnt <- c(res_spat, res_spat, 1)
      }
      out <- RNetCDF::var.get.nc(nc, df$pr, start=c(idx_lon, idx_lat, 1), count = cnt, unpack = TRUE)

      out <- mean(out, na.rm = TRUE)
      return(out)
      RNetCDF::close.nc(nc)
    } else {
      out <- NaN
    }
  }
  modisout <- purrr::map(df, pr_get_SatData)
  df <- dplyr::bind_rows(df) %>%
    dplyr::bind_cols(value = unlist(modisout)) %>%
    tidyr::pivot_wider(names_from = "pr", values_from = "value")
}

