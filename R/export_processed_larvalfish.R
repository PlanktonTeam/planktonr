#' Create all processed data products for Larval Fish
#'
#' @param outD
#'
#' @return
#' @export
#'
#' @examples
#'
#'@importFrom magrittr "%>%"
export_processed_larvalfish <- function(OutD){

  LFCount <- getLFCountAll()
  fwrite(LFCount, file.path(OutD, "LFCount_All.csv"))


  LFCountBGC <- getLFCountBGC()
  fwrite(LFCountBGC, file.path(OutD,"Output/LFCount_BGC.csv"))
}
