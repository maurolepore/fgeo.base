#' Print all rows and columns of a tibble -- as if it was a dataframe.
#'
#' @param x A tibble
#' @param n Integer; number of rows to print.
#' @param width Integer; number of columns to print.
#'
#' @family general miscellaneous functions
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' if (!requireNamespace("tibble")) {
#'   stop(
#'     "To run this example you must first install tibble with:\n",
#'     "install.packages('tibble')",
#'     call. = FALSE
#'   )
#' }
#' tbl <- tibble::tibble(runif(50))
#' print_all(tbl)
#' }
print_all <- function(x, n = nrow(x), width = Inf) {
  if (!any(grepl("tbl", class(x)))) {
    stop("`x` must be a tibble.")
  }
  print(x = x, n = n, width = width)
}

