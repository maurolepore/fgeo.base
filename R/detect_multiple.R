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
#' flag_if
#'
#' @examples
flag_if <- function(.data, predicate, condition = warning, msg = NULL) {
  stopifnot(length(condition) == 1)
  if (predicate(.data)) condition(msg %||% "Flagged values were detected.")
  invisible(.data)
}










flag_predicate_f <- function(predicate, prefix = "Flagged") {
  function(.data, cond, msg = NULL) {
    stopifnot(length(cond) == 1)

    default_msg <- paste0(prefix, " values were detected.")
    if (predicate(.data)) cond(msg %||% default_msg)

    invisible(.data)
  }
}

#' @rdname is_multiple
#' @export
flag_duplicated <- flag_predicate_f(is_duplicated, "Duplicated")

#' @rdname is_multiple
#' @export
flag_multiple <- flag_predicate_f(is_multiple, "Multiple")



#' Factories to detect and flag duplicated and multiple values of a variable.
#'
#' These functions create other functions to detect and flag multiple values of a
#' specific variable. Their goal is to create expressive predicates
#' (`detect_*()`) that can be used in, for example, `if()` statements; and
#' expressive functions to create messages when a condition is true (`flag_*()`).
#'
#' @param name String; the name of a variable of `.data`.
#' @param cond Symbol; the bare name of a function that outputs a condition:
#'   e.g. warning, stop, message, rlang::warn, rlang::abort, rlang::inform.
#'
#' @section Arguments to the resulting function:
#' * `.data`: A dataframe.
#' * `msg`: String; an optional custom
#'   message.
#'
#' @seealso [is_multiple()], [flag_multiple()], [is_duplicated()],
#' [flag_duplicated()].
#'
#' @family functions to check inputs.
#' @family functions for developers.
#' @family predicates.
#' @family function factories.
#'
#' @export
#'
#' @return A function.
#'
#' @examples
#' # DETECT ------------------------------------------------------------------
#' dfm <- data.frame(CensusID = c(1, 2, NA))
#' censusid_has_multiple_values <- detect_multiple_f("censusid")
#' if (censusid_has_multiple_values(dfm)) "Hello world"
#'
#' censusid_has_multiple_values(data.frame(CensusID = c(1, 1, NA)))
#'
#' # Insensitive to upper/lowercase
#' multiple_censusid <- detect_multiple_f("censusid")
#' multiple_censusid(data.frame(censusid = c(1, 2, NA)))
#' multiple_censusid <- detect_multiple_f("CENSUSID")
#' multiple_censusid(data.frame(censusid = c(1, 2, NA)))
#'
#' # FLAG --------------------------------------------------------------------
#' .df <- data.frame(a = 1:3, b = 1, stringsAsFactors = FALSE)
#' flag_multiple_f("a")(.df)
#' flag_multiple_f("a", message)(.df, "Hello world.")
#'
#' # Silent
#' flag_duplicated_f("a")(.df)
#' .df2 <- data.frame(a = c(1, 1, 1), stringsAsFactors = FALSE)
#' flag_duplicated_f("a")(.df2)
#'
#' \dontrun{
#' # Dealing with grouped data
#' if (!requireNamespace("dplyr")) {
#'   library(dplyr)
#'   # `b` is single within groups but multiple accross entire dataset
#'   .df <- tibble(a = c(1, 1, 2, 2), b = c(1, 1, 2, 2))
#'
#'   by_x <- group_by(.df, a)
#'   # Works accross entire dataset -- expect warning
#'   flag_if_multiple_b <- flag_multiple_f("b")
#'   flag_if_multiple_b(by_x)
#'   # Works within groups -- expect silent
#'   do(by_x, (flag_if_multiple_b(.)))
#'   # Same, with interface similar to lapply() and purrr::map()
#'   fgeo.tool::by_group(by_x, flag_if_multiple_b)
#'   # Also consider tidyr::nest() + dplyr::mutate() + dpyr::map())
#' }
#' }
detect_multiple_f <- function(name) {
  force(name)
  name <- tolower(name)
  function(.data) is_multiple(extract_column(.data, name))
}

#' @rdname detect_multiple_f
#' @export
detect_duplicated_f <- function(name) {
 force(name)
 name <- tolower(name)
 function(.data) is_duplicated(extract_column(.data, name))
}





flag_predicate <- function(name, cond = warning, predicate, prefix) {
  force(name)
  force(cond)
  name <- tolower(name)
  function(.data, msg = NULL) {
    msg <- msg %||% paste0(name, ": ", prefix, " values were detected.")
    predicate(extract_column(.data, name), cond = cond, msg = msg)
    invisible(.data)
  }
}

#' @rdname detect_multiple_f
#' @export
flag_multiple_f <- function(name, cond = warning) {
  flag_predicate(name, cond, flag_multiple, "Multiple")
}

#' @rdname detect_multiple_f
#' @export
flag_duplicated_f <- function(name, cond = warning) {
  flag_predicate(name, cond, flag_duplicated, "Duplicated")
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
