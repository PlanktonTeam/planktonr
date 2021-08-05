#' Create all processed data products for Larval Fish
#'
#' @param outD The directory where the output will be saved.
#'
#' @export
#'
#' @examples
#'
#'@importFrom magrittr "%>%"
export_processed_larvalfish <- function(outD){

  LFCount <- getLFCountAll()
  data.table::fwrite(LFCount, file.path(outD, "LFCount_All.csv"))


  LFCountBGC <- getLFCountBGC()
  data.table::fwrite(LFCountBGC, file.path(outD,"Output/LFCount_BGC.csv"))
}
