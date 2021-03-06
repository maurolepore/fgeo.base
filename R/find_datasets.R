#' Find datasets in a package.
#'
#' @param package String; package name to search.
#'
#' @family general functions to find or approximate
#'
#' @return Sting.
#' @export
#'
#' @examples
#' find_datasets("fgeo.base")
find_datasets <- function(package) {
  dinfo <- utils::data(package = package)
  dinfo[["results"]][, "Item"]
}
