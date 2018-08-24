#' Flag if a predicate returns TRUE and throw a condtion with optional message.
#'
#' @param .data Vector.
#' @param predicate A predicate function.
#' @param condition A condition function (e.g. [stop()], [warning()],
#'   `rlang::inform()`).
#' @param msg String. An optional custom message.
#'
#' @return A condition (and `.data` invisibly).
#' @export
#'
#' @examples
#' dupl <- c(1, 1)
#' flag_if(dupl, is_duplicated)
#' # Silent
#' flag_if(dupl, is_multiple)
#'
#' mult <- c(1, 2)
#' flag_if(mult, is_multiple, message, "Custom")
#' # Silent
#' flag_if(mult, is_duplicated)
#'
#' # Both silent
#' flag_if(c(1, NA), is_multiple)
#' flag_if(c(1, NA), is_duplicated)
flag_if <- function(.data, predicate, condition = warning, msg = NULL) {
  stopifnot(length(condition) == 1)
  if (predicate(.data)) condition(msg %||% "Flagged values were detected.")
  invisible(.data)
}
