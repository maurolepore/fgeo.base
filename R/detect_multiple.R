# TODO: Add functions to documentation.

#' Detect and flag multiple values of a variable.
#'
#' * `detect_multiple()` is a predicate function that returns `TRUE` if it
#' detects multiple different values of a variable  (e.g. c(1, 2)`).
#' * `detect_duplicated()` is a predicate function that returns `TRUE` if it
#' detects duplicated values of a variable (e.g. c(1, 1)`).
#' * `flag_multiple()` and `flag_duplicated()` throw a condition when
#' `detect_multiple()` and `detect_duplicated()` would return `TRUE`. They also
#' return the main input, invisibly.
#'
#' @param .data A dataframe.
#' @param name String; the name of a variable of `.data`.
#' @param cond Symbol; the bare name of a function that outputs a condition:
#'   e.g. warning, stop, message, rlang::warn, rlang::abort, rlang::inform.
#' @param msg String; a custom message.
#'
#' @seealso [detect_multiple_f()], [flag_multiple_f()].
#'
#' @family functions to check inputs.
#' @family functions for developers.
#' @family predicates.
#'
#' @return
#' * `detect_multiple()`:A single `TRUE` or `FALSE`. Insensitive to upper-
#' lower-case.
#'
#' @export
#'
#' @examples
#' # DETECT -----------------------------------------------------------------
#' detect_multiple(c(1, 2))
#' detect_multiple(c(1, 1))
#' detect_multiple(c(1, NA))
#'
#' detect_duplicated(c(1, 2))
#' detect_duplicated(c(1, 1))
#' detect_duplicated(c(1, NA))
#'
#' # FLAG -------------------------------------------------------------------
#' duplicated_not_multiple <- c(1, 1, 1)
#' flag_multiple(duplicated_not_multiple, warning)
#' flag_duplicated(duplicated_not_multiple, warning)
#'
#' flag_duplicated(duplicated_not_multiple, warning, "Custom message")
detect_multiple <- function(.data) {
  length(unique(stats::na.omit(.data))) > 1
}

#' @rdname detect_multiple
#' @export
detect_duplicated <- function(.data) {
  any(duplicated(.data))
}

flag_predicate_f <- function(predicate, prefix) {
  function(.data, cond, msg = NULL) {
    stopifnot(length(cond) == 1)

    default_msg <- paste0(prefix, " values were detected.")
    if (predicate(.data)) cond(msg %||% default_msg)

    invisible(.data)
  }
}

#' @rdname detect_multiple
#' @export
flag_duplicated <- flag_predicate_f(detect_duplicated, "Duplicated")

#' @rdname detect_multiple
#' @export
flag_multiple <- flag_predicate_f(detect_multiple, "Multiple")



#' Factories of predicates to detect and flag multiple values of a variable.
#'
#' * `detect_multiple_f()` is a factory of predicate functions that are specific
#' to a particular variable. It's goal is to create expressive and short
#' predicates that can be used in, for example, `if()` statements.
#'
#' @inheritParams detect_multiple
#' @seealso [detect_multiple()], [flag_multiple()].
#'
#' @family functions for developers.
#' @family function factories.
#'
#' @export
#'
#' @examples
#' # DETECT ------------------------------------------------------------------
#' multiple_censusid <- detect_multiple_f("censusid")
#' multiple_censusid(data.frame(CensusID = c(1, 2, NA)))
#' multiple_censusid(data.frame(CensusID = c(1, 1, NA)))
#'
#' # Insensitive to upper/lowercase
#' multiple_censusid <- detect_multiple_f("censusid")
#' multiple_censusid(data.frame(censusid = c(1, 2, NA)))
#' multiple_censusid <- detect_multiple_f("CENSUSID")
#' multiple_censusid(data.frame(censusid = c(1, 2, NA)))
#'
#' # FLAG --------------------------------------------------------------------
#' # On a dataframe
#' .df <- data.frame(a = 1:3, b = 1, stringsAsFactors = FALSE)
#'
#' flag_multiple_f("a")(.df)
#' flag_multiple_f("a", message)(.df, "Hello world.")
#' # Silent
#' flag_multiple_f("b", warning)(.df, "Hello world")
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
  function(.data) detect_multiple(extract_column(.data, name))
}

# TODO: Document
#' @rdname detect_multiple_f
#' @export
detect_duplicated_f <- function(name) {
 force(name)
 name <- tolower(name)
 function(.data) detect_duplicated(extract_column(.data, name))
}

#' @rdname detect_multiple_f
#' @export
flag_multiple_f <- function(name, cond = warning) {
  force(name)
  force(cond)
  name <- tolower(name)
  function(.data, msg = NULL) {
    flag_multiple(extract_column(.data, name), cond = cond, msg = msg)
    invisible(.data)
  }
}

# TODO: Document
#' @rdname detect_multiple_f
#' @export
flag_duplicated_f <- function(name, cond = warning) {
  force(name)
  force(cond)
  name <- tolower(name)
  function(.data, msg = NULL) {
    flag_duplicated(extract_column(.data, name), cond = cond, msg = msg)
    invisible(.data)
  }
}

extract_column <- function(.data, name) {
  stopifnot(is.data.frame(.data))
  .data <- stats::setNames(.data, tolower(names(.data)))
  stopifnot_has_name(.data, name)
  .data[[name]]
}

stopifnot_has_name <- function(.data, name) {
  if (!hasName(.data, name)) stop(name, " is an invalid name", call. = FALSE)
}
