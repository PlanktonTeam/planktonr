
#' Define the constructor function for the planktonr data class
#'
#' @param data The data tibble to be updated to the planktonr class
#' @param type The data type - Microbes, Phytoplankton or Zooplankton
#' @param survey The survey of the data - NRS, CPR, LTM
#' @param variable What variable is being subsetted
#' @param subset NOT USED AT THE MOMENT
#'
#' @returns An object of class planktonr_dat
#' @export
#'
#' @examples
pr_planktonr_class <- function(data, type = NULL, survey = NULL, variable = NULL, subset = NULL) {

  # TODO Add assert to function

  # TODO Can we add Chemistry, Micro, Pico, TSS, CTD etc this - All the NRS BGC ones? (and in pr_get_NRSEnvContour)
  # if (!(type %in% c("Microbes", "Phytoplankton", "Zooplankton", "Water", "EOV"))) {
  #   stop("Invalid Type. Must be 'Microbes', 'Phytoplankton' or 'Zooplankton'.")
  # }

  # Survey - NRS, CPR, Coastal, LTM, "GO-SHIP"

  # Variable - Can we also replace `EOV =` with `Variable =`

  stopifnot(inherits(data, "tbl_df")) # Add input checks.

  if (!"planktonr_dat" %in% class(data)) { # Only do this if data isn't of class planktonr_dat
    tibble::new_tibble(
      data,
      class = c("planktonr_dat", class(data)),
      Type = type,
      Survey = survey,
      Variable = variable,
      Subset = subset,
      Trend = NULL
    )
  } else {
    return(data)
    }

}



test <- c("planktonr_dat", "tbl_df", "tbl", "data.frame")

#' Generic function and method for Type
#'
#' @param x planktonr r data object
#'
#' @returns attribute `Survey`
#' @export
#'
#' @examples
pr_get_type <- function(x){
  UseMethod("pr_get_type")
}



#' Generic function and method for Type
#'
#' @param x planktonr r data object
#'
#' @returns attribute `Type`
#' @export
#'
#' @examples
pr_get_type.planktonr_dat <- function(x){
  attr(x, "Type")
}




#' Generic function and method for Survey
#'
#' @param x planktonr r data object
#'
#' @returns attribute `Survey`
#' @export
#'
#' @examples
pr_get_survey <- function(x){
  UseMethod("pr_get_survey")
}



#' Generic function and method for Survey
#'
#' @param x planktonr r data object
#'
#' @returns attribute `Survey`
#' @export
#'
#' @examples
pr_get_survey.planktonr_dat <- function(x){
  attr(x, "Survey")
}



#' Generic function and method for Variable
#'
#' @param x planktonr r data object
#'
#' @returns attribute `Variable`
#' @export
#'
#' @examples
pr_get_variable <- function(x){
  UseMethod("pr_get_variable")
}



#' Generic function and method for Variable
#'
#' @param x planktonr r data object
#'
#' @returns attribute `Variable`
#' @export
#'
#' @examples
pr_get_variable.planktonr_dat <- function(x){
  attr(x, "Variable")
}



#' print method for planktonr_dat
#'
#' @param x The planktonr dataframe
#' @param ... Other possible objects to be passed to the tidyverse
#'
#' @export
print.planktonr_dat <- function(x, ...) {
  cat("planktonr Data Object\n")
  if(!is.null(attr(x, "Survey"))){cat("Survey:", attr(x, "Survey"), "\n")}
  if(!is.null(attr(x, "Type"))){cat("Type:", attr(x, "Type"), "\n")}
  if(!is.null(attr(x, "Variable"))){cat("Variable:", attr(x, "Variable"), "\n")}
  cat("--------------------\n")
  print(tibble::as_tibble(x)) # print the tibble part.
}

#' pivot_longer method for planktonr_dat
#'
#' @param data  The planktonr data object
#' @param ... Other possible objects to be passed to the tidyverse
#'
#' @export
pivot_longer.planktonr_dat <- function(data, ...) {

  # Call the next method (pivot_longer for tbl_df)
  NextMethod("pivot_longer") %>%
    pr_planktonr_class(type = pr_get_type(data),
                       survey = pr_get_survey(data),
                       variable = pr_get_variable(data),
                       # subset = pr_get_type(data),
    ) # Re-apply the plankton_data class

}



#' pivot_longer method for planktonr_dat
#'
#' @param data  The planktonr data object
#' @param ... Other possible objects to be passed to the tidyverse
#'
#' @export
pivot_wider.planktonr_dat <- function(data, ...) {

  # Call the next method (pivot_wider for tbl_df)
  NextMethod("pivot_wider") %>%
    pr_planktonr_class(type = pr_get_type(data),
                       survey = pr_get_survey(data),
                       variable = pr_get_variable(data),
                       # subset = pr_get_type(data),
    ) # Re-apply the plankton_data class

}


# # Define a method for using....
# group_by.planktonr_dat <- function(data, ..., .add = FALSE) {
#
#   # Call the next method
#   NextMethod("group_by", ..., .add = .add) %>%
#     pr_planktonr_class(type = pr_get_type(data),
#                        survey = pr_get_survey(data),
#                        variable = pr_get_variable(data),
#                        # subset = pr_get_type(data),
#     ) # Re-apply the plankton_data class
#
# }



#' summarise method for planktonr_dat
#'
#' @param data The planktonr data object
#' @param ... Other possible objects to be passed to the tidyverse
#' @param .by Group by
#' @param .groups Should the groups be dropped
#'
#' @export
summarise.planktonr_dat <- function(data, ..., .by, .groups = "drop") {

  # Call the next method
  NextMethod("summarise", .by = tidyselect::all_of(.by), .groups = .groups) %>%
    pr_planktonr_class(type = pr_get_type(data),
                       survey = pr_get_survey(data),
                       variable = pr_get_variable(data),
                       # subset = pr_get_type(data),
    ) # Re-apply the plankton_data class

}


#' drop_na method for planktonr_dat
#'
#' @param data The planktonr data object
#' @param ... Other possible objects to be passed to the tidyverse
#'
#' @export
drop_na.planktonr_dat <- function(data, ...) {

  # Call the next method
  NextMethod() %>%
    pr_planktonr_class(type = pr_get_type(data),
                       survey = pr_get_survey(data),
                       variable = pr_get_variable(data),
                       # subset = pr_get_type(data),
    ) # Re-apply the plankton_data class

}




#' Check and update Type as required
#'
#' @param Type Data type
#'
#' @returns Type as a standardised string
#'
pr_check_type <- function(Type){

  Type = stringr::str_to_lower(Type)

  if (Type %in% c("p", "phyto", "phytoplankton")){Type = "Phytoplankton"}
  if (Type %in% c("z", "zoop", "zooplankton")){Type = "Zooplankton"}
  if (Type %in% c("w", "water")){Type = "Water"}

  return(Type)
}


.onLoad <- function(libname, pkgname) {
  # Register the print method for my_class
  vctrs::s3_register("base::print", "planktonr_dat")

  # Register a tidyr method example.
  if (requireNamespace("tidyr", quietly = TRUE)) {
    vctrs::s3_register("tidyr::drop_na", "planktonr_dat")
    vctrs::s3_register("tidyr::pivot_longer", "planktonr_dat")
    vctrs::s3_register("tidyr::pivot_wider", "planktonr_dat")
  }

  # Register a dplyr method example.
  if (requireNamespace("dplyr", quietly = TRUE)) {
    vctrs::s3_register("dplyr::summarise", "planktonr_dat")
  }
}

# Convert to planktonr class
# dat <- pr_planktonr_class(dat, type = Type, survey = Survey, variable = Variable, subset = Subset)


