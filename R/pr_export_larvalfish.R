# Create all processed data products for Larval Fish
#
# @param outD The directory where the output will be saved.
#
# @export
#
# @examples
# \dontrun{
# pr_export_larvalfish("Output")
# }
# @importFrom rlang .data
# pr_export_larvalfish <- function(outD){
#
#   LFCount <- pr_get_LFCountAll()
#   data.table::fwrite(LFCount, file.path(outD, "NRS_LarvalFish_All.csv"))
#
#
#   LFCountBGC <- pr_get_LFCountBGC()
#   data.table::fwrite(LFCountBGC, file.path(outD,"NRS_LarvalFish_BGC.csv"))
# }
