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


#' Pipe operator
#'
#' @name %m+%
#' @rdname ops-m+
#' @keywords internal
#' @export
#' @importFrom lubridate %m+%
#' @usage lhs \%m+\% rhs
#' @param lhs A period or a date-time object of class.
#' @param rhs A period or a date-time object of class.
#' @return The result of calling `rhs(lhs)`.
NULL
