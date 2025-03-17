
#' Define the constructor function for the planktonr data class
#'
#' @param data The data tibble to be updated to the planktonr class
#' @param type The data type - Microbes, Phytoplankton or Zooplankton
#' @param source The source of the data - NRS, CPR, LTM
#'
#' @returns An object of class planktonr_dat
#' @export
#'
#' @examples
create_planktonr_dat <- function(data, type, source) {

  # TODO Add assert to function
  if (!(type %in% c("Microbes", "Phytoplankton", "Zooplankton"))) {
    stop("Invalid tag. Must be 'Microbes', 'Phytoplankton' or 'Zooplankton'.")
  }

  stopifnot(inherits(data, "tbl_df")) # Add input checks.

  structure(
    data,
    class = c("planktonr_dat", "tbl_df", "tbl", "data.frame"),
    Type = type,
    Source = source
  )
}



#' Generic function and method for Type
#'
#' @param x
#'
#' @returns attribute `Source`
#' @export
#'
#' @examples
get_planktonr_type <- function(x){
  UseMethod("get_planktonr_type")
}



#' Generic function and method for Type
#'
#' @param x planktonr r data object
#'
#' @returns attribute `Type`
#' @export
#'
#' @examples
get_planktonr_type.planktonr_dat <- function(x){
  attr(x, "Type")
}




#' Generic function and method for Source
#'
#' @param x planktonr r data object
#'
#' @returns attribute `Source`
#' @export
#'
#' @examples
get_planktonr_source <- function(x){
  UseMethod("get_planktonr_source")
}



#' Generic function and method for Source
#'
#' @param x planktonr r data object
#'
#' @returns attribute `Source`
#' @export
#'
#' @examples
get_planktonr_source.planktonr_dat <- function(x){
  attr(x, "Source")
}




#' Example of a custom print method.
#'
#' @param x planktonr r data object
#' @param ... further arguments passed to or from other methods.
#'
#' @returns
#' @export
#'
#' @examples
print.planktonr_dat <- function(x, ...) {
  cat("planktonr Data Object\n")
  cat("Source:", attr(x, "Source"), "\n")
  cat("Type:", attr(x, "Type"), "\n")
  cat("--------------------\n")
  print(tibble::as_tibble(x)) # print the tibble part.
}

