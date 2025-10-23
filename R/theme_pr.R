#' Define theme_pr() function
#'
#' @importFrom ggplot2 '%+replace%'
#'
#' @param base_size use to set font size with base_size
#'
#' @export
#' @return planktonr theme
#'
#' @examples
#' \dontrun{ggplot() + geom_blank() + theme_pr()}
theme_pr <- function(base_size = NULL){

  # The default relative font sizes in ggplot2 are controlled by the
  # base_size parameter (default is 11) and the rel() function. Here
  # are the standard relative sizes for different text elements:
  #
  # plot.title: rel(1.2) - 120% of base size
  # plot.subtitle: rel(1.0) or rel(0.9) - 90-100% of base size (depending on theme)
  # axis.title: rel(1.0) - 100% of base size
  # axis.text: rel(0.8) - 80% of base size
  # legend.title: rel(0.9) - 90% of base size
  # legend.text: rel(0.8) - 80% of base size
  # strip.text (facet labels): rel(0.8) - 80% of base size
  # plot.caption: rel(0.8) - 80% of base size

  # Check for global option first, then use default if not set
  if (is.null(base_size)) {
    base_size <- getOption("planktonr.base_size", default = 15)
  }

  font <- "Georgia"   #assign font family up front
  ggplot2::theme_bw(base_size = base_size) %+replace%    # replace elements we want to change
    ggplot2::theme(

      # Modify the legend
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = ggplot2::rel(1.0)),
      legend.title = ggplot2::element_text(size = ggplot2::rel(1.1), face = "bold"),
      legend.justification = c(0.5, 0.5), # Centres the legend

      #grid elements
      # panel.grid.major = element_blank(),    #strip major gridlines
      # panel.grid.minor = element_blank(),    #strip minor gridlines
      # axis.ticks = element_blank(),          #strip axis ticks

      # facet elements
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(hjust = 0, vjust = 0.5),

      #since theme_minimal() already strips axis lines,
      #we don't need to do that again

      #text elements
      plot.title = ggplot2::element_text(             #title
      #   family = font,            #set font family
      #   size = 20,                #set font size
      #   face = 'bold',            #bold typeface
      #   hjust = 0,                #left align
        vjust = 0.5),               # centre

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

      # Axis Text
      axis.text = ggplot2::element_text(size = ggplot2::rel(0.9)),

      # axis.text.x = element_text(            #margin for axis text
      #   margin=margin(5, b = 10))

      #since the legend often requires manual tweaking
      #based on plot content, don't define it here
    )
}
