#' Detect and flag multiple values of a variable.
#'
#' * `detect_multiple()` is a predicate function that returns `TRUE` if it
#' detects multiple different values of a variable.
#' * `flag_multiple()` throws a condition and, invisibly, the input data.
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
#' # FLAG -------------------------------------------------------------------
#' # On a vector
#' # Silent
#' single_value <- c(1, 1, 1)
#' flag_multiple(single_value, warning)
#'
#' num <- c(1:3)
#' flag_multiple(num, warning)
#' chr <- c(letters[1:3])
#' flag_multiple(chr, message, "Hello world.")
detect_multiple <- function(.data) {
  length(unique(stats::na.omit(.data))) > 1
}

#' @rdname detect_multiple
#' @export
flag_multiple <- function(.data, cond, msg = NULL) {
  stopifnot(length(cond) == 1)

  customized <- c("Multiple values were detected.\n", msg)
  if (detect_multiple(.data)) cond(msg %||% customized)

  invisible(.data)
}



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
#' @family predicates.
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
  function(.data) {
    stopifnot(is.data.frame(.data))
    .data <- stats::setNames(.data, tolower(names(.data)))
    .var <- .data[[name]]
    stopifnot_has_name(.data, name)
    detect_multiple(.var)
  }
}

#' @rdname detect_multiple_f
flag_multiple_f <- function(name, cond = warning) {
  force(name)
  force(cond)
  name <- tolower(name)
  function(.data, msg = NULL) {
    stopifnot(is.data.frame(.data))
    .data <- stats::setNames(.data, tolower(names(.data)))
    x <- .data[[name]]
    stopifnot_has_name(.data, name)
    flag_multiple(.data = x, cond = cond, msg = msg)

    invisible(.data)
  }
}


stopifnot_has_name <- function(.data, name) {
  if (!hasName(.data, name)) stop(name, " is an invalid name", call. = FALSE)
}























#' Flag if a vector or dataframe column meets a condition.
#'
#' These funcitons are useful to create messagees, warnings and error messages
#' based on a condition.
#'
#' @param x,.x A vector or dataframe.
#' @param .x_var String giving a column name of `.x`.
#' @param .if A condition on `x` or .x_var.
#' @param .flag A function that throws a condition e.g. `stop`, `warning`,
#'   `message`, `rlang::abort`, `rlang::warn`, `rlang::inform`.
#' @param msg An optional custom message.
#'
#' @family functions for developers.
#'
#' @return Throws a condition determined by `.flag` or invisible.
#' @examples
#' dupl <- c(1, 1)
#' # Flags
#' is_duplicated <- function(x) any(duplicated(x))
#' flag_vector_if(dupl, is_duplicated(dupl), message)
#' flag_vector_if(dupl, is_duplicated(dupl), message, "Duplicated values.")
#' # Silent
#' multiple_values <- function(x) length(unique(x)) > 1
#' flag_vector_if(dupl, multiple_values(dupl), message)
#' flag_vector_if(dupl, multiple_values(dupl), message, "Multiple values.")
#'
#' mult <- c(1, 2)
#' # Silent
#' flag_vector_if(mult, is_duplicated(mult), message)
#' flag_vector_if(mult, is_duplicated(mult), message, "Duplicated values.")
#'
#' # Flags
#' flag_vector_if(mult, multiple_values(mult), message)
#' flag_vector_if(mult, multiple_values(mult), message, "Multiple values.")
#'
#' dupl <- c(1, 1)
#' dupl_df <- data.frame(dupl)
#' # Flags
#' is_duplicated <- function(x) any(duplicated(x))
#' flag_if(dupl_df, "dupl", is_duplicated(dupl), message)
#' flag_if(dupl_df, "dupl", is_duplicated(dupl), message, "Duplicated values.")
#' # Silent
#' multiple_values <- function(x) length(unique(x)) > 1
#' flag_if(dupl_df, "dupl", multiple_values(dupl), message)
#' flag_if(dupl_df, "dupl", multiple_values(dupl), message, "Multiple values.")
#'
#' mult_df <- data.frame(mult)
#' # Silent
#' flag_if(mult_df, "mult", is_duplicated(mult), message)
#' flag_if(mult_df, "mult", is_duplicated(mult), message, "Duplicated values.")
#' # Flags
#' flag_if(mult_df, "mult", multiple_values(mult), message)
#' flag_if(mult_df, "mult", multiple_values(mult), message, "Multiple values.")
#' @keywords internal
#' @noRd
flag_if <- function(.data, name, .if, .flag = warning, msg = NULL) {
  stopifnot(is.data.frame(.data))
  stopifnot_has_name(.data, name)

  name <- .data[[name]]
  flag_vector_if(name, .if, .flag, msg)

  invisible(.data)
}

flag_vector_if <- function(x, .if, .flag, msg = NULL) {
  stopifnot(length(.flag) == 1)
  customized <- c("Flagged values were detected.\n", msg)
  if (.if) {
    .flag(msg %||% customized)
  }
  invisible(x)
}
