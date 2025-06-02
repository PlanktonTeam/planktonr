# R/zzz.R

.onLoad <- function(libname, pkgname) {
  # Register the print method for my_class
  vctrs::s3_register("base::print", "planktonr_dat")

  # Register tidyr methods
  if (requireNamespace("tidyr", quietly = TRUE)) {
    vctrs::s3_register("tidyr::pivot_longer", "planktonr_dat")
    vctrs::s3_register("tidyr::pivot_wider", "planktonr_dat")
    vctrs::s3_register("tidyr::drop_na", "planktonr_dat")
  }

  # Register dplyr methods
  if (requireNamespace("dplyr", quietly = TRUE)) {
    vctrs::s3_register("dplyr::summarise", "planktonr_dat")
    vctrs::s3_register("dplyr::group_by", "planktonr_dat")
    vctrs::s3_register("dplyr::mutate", "planktonr_dat")
    vctrs::s3_register("dplyr::filter", "planktonr_dat")
    vctrs::s3_register("dplyr::select", "planktonr_dat")
    vctrs::s3_register("dplyr::arrange", "planktonr_dat")
  }
}
