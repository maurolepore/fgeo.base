# Check -------------------------------------------------------------------

#' Report if a vector or a variable of a dataframe is duplicated.
#'
#' @param x A dataframe.
#' @param x_var String; the name of a variable of `x`.
#' @param v A vector.
#' @param cond String; the name of a function that outputs a condition: one of
#'   "warning", "stop", "message".
#' @param msg String; a custom message.
#'
#' @return Invisible `v` or a condition and a message.
#' @family functions to check inputs.
#' @family functions for developers.
#' @export
#'
#' @examples
#' # On a vector
#' unique_v <- rep(1, 3)
#' num <- c(1:3)
#' chr <- c(letters[1:3])
#' flag_multiple_vector(unique_v, warning)
#' flag_multiple_vector(num, warning)
#' flag_multiple_vector(chr, message, "Hello world.")
#'
#' # On a dataframe
#' .df <- data.frame(a = 1:3, b = 1, stringsAsFactors = FALSE)
#'
#' flag_multiple(.df, "a")
#' flag_multiple(.df, "a", message, "Hello world.")
#' # Silent
#' flag_multiple(.df, "b", warning, "Hello world")
#'
#' \dontrun{
#' # Dealing with grouped data
#' if (!requireNamespace("dplyr")) {
#'   # `b` is single within groups but multiple accross entire dataset
#'   .df <- data.frame(
#'     a = c(1, 1, 2, 2), b = c(1, 1, 2, 2),
#'     stringsAsFactors = FALSE
#'   )
#'
#'   by_x <- dplyr::group_by(.df, a)
#'   # Works accross entire dataset
#'   flag_multiple(by_x, "b")
#'   # Works within groups entire dataset
#'   dplyr::do(by_x, (flag_multiple(., "b")))
#'   # Also consider tidyr::nest() + dplyr::mutate() + dpyr::map())
#' }
#' }
flag_multiple <- function(x, x_var, cond = warning, msg = NULL) {
  stopifnot(is.data.frame(x))
  if (!x_var  %in% names(x)) stop(x_var, " is an invalid name", call. = FALSE)

  x_var <- x[[x_var]]
  flag_multiple_vector(v = x_var, cond = cond, msg = msg)

  invisible(x)
}

#' @rdname flag_multiple
#' @export
flag_multiple_vector <- function(v, cond, msg = NULL) {
  stopifnot(length(cond) == 1)

  customized <- c("Multiple values were detected.\n", msg)
  if (length(unique(v)) > 1) {
    cond(msg %||% customized)
  }

  invisible(v)
}
