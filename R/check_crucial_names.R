#' Check if an object contains specific names.
#'
#' @param x A named object.
#' @param nms String; names expected to be found in `x`.
#'
#' @family functions to check inputs.
#' @family functions for developers.
#' @return Invisible `x`, or an error with informative message.
#' @export
#'
#' @examples
#' v <- c(x = 1)
#' check_crucial_names(v, "x")
#'
#' dfm <- data.frame(x = 1)
#' check_crucial_names(dfm, "x")
check_crucial_names <- function(x, nms) {
  stopifnot(is_named(x))
  stopifnot(is.character(nms))

  are_names_expected <- all(nms %in% names(x))
  if (are_names_expected) {
    return(invisible(x))
  } else {
    stop(
      "Ensure your data set has these variables:\n", commas(nms),
      call. = FALSE
    )
  }
}

is_named <- function (x) {
  nms <- names(x)
  if (is.null(nms)) {
    return(FALSE)
  }
  if (any(nms == "" | is.na(nms))) {
    return(FALSE)
  }
  TRUE
}
