#' Setting the colours for plots using cmocean
#'
#' @param pal is the palette name
#' @param n is the number of colours required
#'
#' @return is a list of colours of length n from palette pal
#' @export
#'
#' @importFrom cmocean cmocean
#'
#' @examples
#' plotCols <- pr_get_PlotCols('matter', 5)
pr_get_PlotCols <- function(pal, n){
  plotCols <- cmocean::cmocean(pal)(n)
  return(plotCols)
}


#' To product the base map of Australia for plotting
#'
#' @return an sf object for plotting the base map of Australia
#' @export
#'
#' @import sf
#' @import rgeos
#'
#' @examples
#' MapOz <- pr_get_MapOz()
pr_get_MapOz <- function(){
  MapOz <- rnaturalearth::ne_countries(scale = "medium", country = "Australia",
                                       returnclass = "sf")
  return(MapOz)
}
