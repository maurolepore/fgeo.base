#' Find datasets in a package.
#'
#' @param package
#'
#' @return
#' @export
#'
#' @examples
find_datasets <- function(package) {
  dinfo <- utils::data(package = package)
  dinfo[["results"]][, "Item"]
}
