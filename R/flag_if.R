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
#'
#'
#'
flag_if <- function(.data, ...) {
  UseMethod("flag_if")
}

flag_if.default <- function(.data, predicate, condition = warning, msg = NULL) {
  stopifnot(length(condition) == 1)
  if (predicate(.data)) condition(msg %||% "Flagged values were detected.")
  invisible(.data)
}

flag_if.data.frame <- function(.data, name, predicate, condition = warning, msg = NULL) {
  name <- tolower(name)
  msg <- msg %||% paste0(name, ": Flagged values were detected.")
  flag_if(extract_column(.data, name), predicate, condition, msg)
  invisible(.data)
}

#' @rdname flag_if
#' @export
detect_if <- function(.data, name, predicate) {
  name <- tolower(name)
  predicate(extract_column(.data, name))
}

extract_column <- function(.data, name) {
  stopifnot(is.data.frame(.data))
  .data <- stats::setNames(.data, tolower(names(.data)))
  stopifnot_has_name(.data, name)
  .data[[name]]
}

stopifnot_has_name <- function(.data, name) {
  if (!utils::hasName(.data, name)) {
    stop(name, " is an invalid name", call. = FALSE)
  }
}
