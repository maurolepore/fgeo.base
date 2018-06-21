#' Collapse strings into a single one.
#'
#' These functions are a shortcut for paste0(..., collapse = "<somethig>").
#'
#' @inheritDotParams base::paste
#'
#' @return String.
#' @export
#'
#'
#'
#' @examples
#' glue_comma(1:3)
#' glue_pipe(1:3)
glue_comma <- function(...) {
  paste0(..., collapse = ", ")
}

#' @export
#' @rdname glue_comma
glue_pipe <- function(...) {
  paste0(..., collapse = "|")
}

