utils::globalVariables(".")

#' Get data files available from SOTS moorings
#' @param Type Physical or nutrients
#'
#' @return List of variables available from SOTS files
#' @export
#'
#' @examples
#' df <- pr_get_SOTSvariables(Type = 'Physical')

pr_get_SOTSvariables <- function(Type = 'Physical'){

  if(Type == 'Nutrients'){
    years <- seq(1997, lubridate::year(Sys.Date()), 1)

    nutsFiles <- function(years){
      thredds_url_nuts <- paste0("https://thredds.aodn.org.au/thredds/catalog/IMOS/DWM/SOTS/", years, "/catalog.html")
      catalog_html <- RCurl::getURL(thredds_url_nuts)
      catalog_parsed <- rvest::read_html(catalog_html)
      file_nodes <- rvest::html_nodes(catalog_parsed, "a[href]")
      file_list <- rvest::html_attr(file_nodes, "href")
      file_list[grepl("*RAS*", file_list)]  # Assuming you are looking for .nc files
    }

    file_list <- (purrr::compact(purrr::map(years, nutsFiles)) %>%
                         purrr::map_df(tibble::as_tibble))$value

  } else {
    thredds_url <- "https://thredds.aodn.org.au/thredds/catalog/IMOS/DWM/SOTS/derived_products/gridded/catalog.html"
    catalog_html <- RCurl::getURL(thredds_url)
    catalog_parsed <- rvest::read_html(catalog_html)
    file_nodes <- rvest::html_nodes(catalog_parsed, "a[href]")
    file_list <- rvest::html_attr(file_nodes, "href")
    file_list <- file_list[grepl("\\.nc$", file_list)]

  }

  varlist <- function(file_list){
    # Construct the full URL if needed
    full_url <- paste0("https://thredds.aodn.org.au/thredds/dodsC/", sub(".*=", "", file_list))
    # Open the NetCDF file
    nc <- ncdf4::nc_open(full_url)
    dimensions <- data.frame(names(nc$var))
    ncdf4::nc_close(nc)
    dimensions
  }

  SOTSvariables <- purrr::map(file_list, varlist) %>%
    purrr::list_rbind() %>% dplyr::distinct()

}

#' Get data available from SOTS moorings (limited to use in BOO atm, other variables could be added, select from pr_get_SOTSvariables)
#' @param Type Nutrients or Physical
#'
#' @return SOTS nutrients or physical parameters data
#' @export
#'
#' @examples
#' df <- pr_get_SOTSMoorData(Type = 'Physical')

pr_get_SOTSMoorData <- function(Type = 'Physical'){

  if(Type == 'Nutrients'){
    years <- seq(1997, lubridate::year(Sys.Date()), 1)

    nutsFiles <- function(years){
      thredds_url_nuts <- paste0("https://thredds.aodn.org.au/thredds/catalog/IMOS/DWM/SOTS/", years, "/catalog.html")
      catalog_html <- RCurl::getURL(thredds_url_nuts)
      catalog_parsed <- rvest::read_html(catalog_html)
      file_nodes <- rvest::html_nodes(catalog_parsed, "a[href]")
      file_list <- rvest::html_attr(file_nodes, "href")
      file_list[grepl("*RAS*", file_list)]  # Assuming you are looking for .nc files
    }

    file_list <- (purrr::compact(purrr::map(years, nutsFiles)) %>%
                    purrr::map_df(tibble::as_tibble))$value

  } else {
    thredds_url <- "https://thredds.aodn.org.au/thredds/catalog/IMOS/DWM/SOTS/derived_products/gridded/catalog.html"
    catalog_html <- RCurl::getURL(thredds_url)
    catalog_parsed <- rvest::read_html(catalog_html)
    file_nodes <- rvest::html_nodes(catalog_parsed, "a[href]")
    file_list <- rvest::html_attr(file_nodes, "href")
    file_list <- file_list[grepl("\\.nc$", file_list)]

  }

  SOTSdata <- function(file_list){
    # Construct the full URL if needed
    full_url <- paste0("https://thredds.aodn.org.au/thredds/dodsC/", sub(".*=", "", file_list))
    # Open the NetCDF file
    nc <- ncdf4::nc_open(full_url)

    dimensions <- names(nc$var)

    variables <- c("NOMINAL_DEPTH_TEMP", "TEMP","NOMINAL_DEPTH_CPHL", 'CPHL', # "NOMINAL_DEPTH_SST", "SST",
                   "NOMINAL_DEPTH_PSAL", 'PSAL', 'MLD', #'PAR', 'NOMINAL_DEPTH_PAR', "NOMINAL_DEPTH_DOX2", 'DOX2',
                   'pHt', 'NOMINAL_DEPTH_pHt', 'NTRI_CONC', "NTRI", "PHOS_CONC", "PHOS", "SLCA_CONC", "SLCA",
                   "ALKA_CONC", "TALK","TCO2", "DEPTH", "NOMINAL_DEPTH")

    variablesAvailable <- intersect(dimensions, variables) %>% stringr::str_subset(pattern = 'DEPTH', negate = TRUE)

    dates <- as.POSIXct((nc$dim$TIME)$vals*3600*24, origin = '1950-01-01 00:00:00', tz = 'UTC')

    vardat <- function(variablesAvailable){
      if('MLD' %in% variablesAvailable){
        vdat <- data.frame(ncdf4::ncvar_get(nc, varid = variablesAvailable),
                           dates)
        colnames(vdat) <- c('0', 'SampleTime_Local')
        vdat %>% tidyr::pivot_longer(-.data$SampleTime_Local, values_to = 'Values', names_to = 'SampleDepth_m') %>%
          dplyr::mutate(SampleDepth_m = as.numeric(.data$SampleDepth_m),
                        Parameters = paste0(variablesAvailable))
      } else {
        if(length(stringr::str_subset(pattern = 'NOMINAL_DEPTH_', dimensions)) > 0) {
          Depthvars <- ncdf4::ncvar_get(nc, varid=paste0("NOMINAL_DEPTH_", variablesAvailable))}
        else if(length(stringr::str_subset(pattern = 'NOMINAL_DEPTH', dimensions)) > 0) {
          Depthvars <- ncdf4::ncvar_get(nc, varid=paste0("NOMINAL_DEPTH"))}
        else {
          Depthvars <- ncdf4::ncvar_get(nc, varid=paste0("DEPTH"))}

        vdat <- data.frame(ncdf4::ncvar_get(nc, varid = variablesAvailable), SampleTime_Local = dates)
        if(grepl('ncdf', colnames(vdat[1]))){names(vdat) = c('X1', 'SampleTime_Local')}
        qcdat <- data.frame(ncdf4::ncvar_get(nc, varid = paste0(variablesAvailable, "_quality_control")), SampleTime_Local = dates)
        if(grepl('ncdf', colnames(qcdat[1]))){names(qcdat) = c('X1', 'SampleTime_Local')}
        qcdat <- qcdat %>%
          tidyr::pivot_longer(-.data$SampleTime_Local, values_to = 'Flags', names_to = 'SampleDepth_m')
        vdat %>%
          tidyr::pivot_longer(-.data$SampleTime_Local, values_to = 'Values', names_to = 'SampleDepth_m') %>%
          dplyr::left_join(qcdat, by = c('SampleDepth_m', 'SampleTime_Local')) %>%
          dplyr::filter(.data$Flags %in% c(1, 2)) %>%
          dplyr::mutate(SampleDepth_m = Depthvars[as.numeric(gsub("[^0-9]", "", .data$SampleDepth_m))],
                        Parameters = paste0(variablesAvailable)) %>%
          dplyr::summarise(Values = sum(.data$Values, na.rm = TRUE), .by = setdiff(colnames(.), "Values"))
      }
    }

    df <- purrr::map(variablesAvailable, vardat) %>%
      purrr::list_rbind()

    ncdf4::nc_close(nc)

    df <- df %>%
      dplyr::mutate(StationName = 'Southern Ocean Time Series',
                    StationCode = 'SOTS',
                    Month_Local = lubridate::month(.data$SampleTime_Local),
                    SampleTime_Local = lubridate::floor_date(.data$SampleTime_Local, unit = 'day'),
                    Parameters = dplyr::case_when(grepl("CPHL", .data$Parameters) ~ "ChlF_mgm3",
                                                  grepl("DOX2", .data$Parameters) ~ "DissolvedOxygen_umolkg",
                                                  grepl("MLD", .data$Parameters) ~ "MLD_m",
                                                  grepl("PSAL", .data$Parameters) ~ "Salinity",
                                                  grepl("TEMP", .data$Parameters) ~ "Temperature_degC",
                                                  grepl("NTRI", .data$Parameters) ~ "Nitrate_umolL",
                                                  grepl("PHOS", .data$Parameters) ~ "Phosphate_umolL",
                                                  grepl("SLCA", .data$Parameters) ~ "Silicate_umolL",
                                                  grepl("TALK|ALKA", .data$Parameters) ~ "Alkalinity_umolkg",
                                                  grepl("pHt", .data$Parameters) ~ "pH",
                                                  grepl("TCO", .data$Parameters) ~ "DIC_umolkg",
                                                  .default = .data$Parameters)) %>%
      dplyr::group_by(.data$StationName, .data$StationCode, .data$Month_Local, .data$SampleTime_Local, .data$Parameters, .data$SampleDepth_m) %>%
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       .groups = 'drop') %>%
      dplyr::mutate(Year_Local = lubridate::year(.data$SampleTime_Local),
                    SampleDepth_m = round(.data$SampleDepth_m/10, 0)*10) %>%
      dplyr::filter(.data$SampleDepth_m %in% c(0, 30, 50, 100, 200, 500)) %>%
      tidyr::drop_na(.data$Parameters, .data$Values)

  }

  df <- purrr::map(file_list, SOTSdata) %>%
    purrr::list_rbind() %>%
    planktonr::planktonr_dat("Water", "SOTS")

}

