#' Default value for `NULL`.
#'
#' This infix function makes it easy to replace `NULL`s with a default value.
#' It's inspired by the way that Ruby's or operation (||) works. It is slightly
#' adapted from `rlang::`%||%`.
#'
#' @param x,y If x is NULL, will return y; otherwise returns x.
#'
#' @examples
#' 1 %||% 2
#' NULL %||% 2
#' @name %>%
#' @rdname pipe
NULL

#' @rdname pipe
#' @export
`%||%` <- function (x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
