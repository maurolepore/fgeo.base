#' Pull names that match a character vector.
#'
#' @param x A named object.
#' @param .match A character vector giving names to match.
#'
#' @family functions dealing with names.
#' @family functions for developers.
#'
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' pull_name_matches(luquillo_stem_random_tiny, c("x", "PX", "gx"))
#' pull_name_matches(luquillo_vft_4quad, c("x", "PX", "gx"))
#' pull_name_matches(luquillo_vft_4quad, c("PY", "PX", "gx", "gy"))
pull_name_matches <- function(x, .match) {
  stopifnot(is_named(x))
  names(x)[grepl(glue_pipe(enline(.match)), names(x))]
}
