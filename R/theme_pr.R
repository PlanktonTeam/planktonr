#

#' Define theme_pr() function
#'
#' @importFrom ggplot2 '%+replace%'
#'
#' @param FontSize use to set font size with base_size
#'
#' @export
#' @return planktonr theme
#'
#' @examples
#' \dontrun{
#' ggplot() +
#'   geom_blank() +
#'   theme_pr()
#' }
theme_pr <- function(FontSize = 16) {
  font <- "Georgia" # assign font family up front
  ggplot2::theme_bw(base_size = FontSize) %+replace% # replace elements we want to change
    ggplot2::theme(
      legend.position = "bottom",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(hjust = 0, vjust = 0.5),
      plot.title = ggplot2::element_text(vjust = 0.5)


      # grid elements
      # panel.grid.major = element_blank(),    #strip major gridlines
      # panel.grid.minor = element_blank(),    #strip minor gridlines
      # axis.ticks = element_blank(),          #strip axis ticks

      # since theme_minimal() already strips axis lines,
      # we don't need to do that again

      # text elements
      # plot.title = element_text(             #title
      #   family = font,            #set font family
      #   size = 20,                #set font size
      #   face = 'bold',            #bold typeface
      #   hjust = 0,                #left align
      #   vjust = 2),               #raise slightly

      # plot.subtitle = element_text(          #subtitle
      #   family = font,            #font family
      #   size = 14),               #font size

      # plot.caption = element_text(           #caption
      #   family = font,            #font family
      #   size = 9,                 #font size
      #   hjust = 1),               #right align

      # axis.title = element_text(             #axis titles
      #   family = font,            #font family
      #   size = 10),               #font size

      # axis.text = element_text(              #axis text
      #   family = font,            #axis famuly
      #   size = 9),                #font size

      # axis.text.x = element_text(            #margin for axis text
      #   margin=margin(5, b = 10))

      # since the legend often requires manual tweaking
      # based on plot content, don't define it here
    )
}
