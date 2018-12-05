#' Throw condition if variable has missing values.
#'
#' @param x A vector or dataframe.
#'
#' @family functions to throw conditions
#'
#' @return A warning message and invisible `x`.
#'
#' @examples
#' warn_na(c(x = 1, y = NA))
#' inform_na(data.frame(x = 1, y = NA))
#' @name condition_na
NULL

condition_na <- function(.f) {
  force(.f)
  function(x) {
    stopifnot(is.data.frame(x) || is.vector(x), !is.null(names(x)), is_named(x))

    out <- vapply(x, function(x) any(is.na(x)), logical(1))
    has_na <- out
    if (any(has_na)) {
      .f("Detected missing values in: ", glue_comma(names(x)[has_na]))
    }
    invisible(x)
  }
}

#' @export
#' @rdname condition_na
warn_na <- condition_na(warning)

#' @export
#' @rdname condition_na
inform_na <- condition_na(message)

#' @export
#' @rdname condition_na
abort_na <- condition_na(stop)
