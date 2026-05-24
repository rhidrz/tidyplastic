#' Load Plastic Waste Cleanup Data
#'
#' Loads the Break Free From Plastic dataset from a local CSV file.
#' The CSV should be placed in the \code{inst/extdata/} folder of
#' the package.
#'
#' @return A tibble with columns including \code{country},
#'   \code{year}, \code{volunteers}, \code{grand_total},
#'   and plastic type columns (hdpe, ldpe, o, pet, pp, ps, pvc).
#' @importFrom readr read_csv
#' @export
load_data <- function() {
  readr::read_csv(path, show_col_types = FALSE)
}
