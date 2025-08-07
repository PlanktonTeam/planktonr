#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Pipe operator for adding time periods to dates
#'
#' @name %m+%
#' @rdname mplus
#' @keywords internal
#' @export
#' @importFrom lubridate %m+%
#' @usage e1 \%m+\% e2
#' @param e1 A period or a date-time object of class.
#' @param e2 A period or a date-time object of class.
#' @return A date-time object of class POSIXlt, POSIXct or Date.
NULL
