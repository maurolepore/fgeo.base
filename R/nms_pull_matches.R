#' Pull names that match a character vector.
#'
#' The `nms_` prefix matches functions in other fgeo packages.
#'
#' @param x A named object.
#' @param .match A character vector giving names to match.
#'
#' @family general functions to deal with names
#' @family functions for developers
#'
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' nms_pull_matches(luquillo_stem_random_tiny, c("x", "PX", "gx"))
#' nms_pull_matches(luquillo_vft_4quad, c("x", "PX", "gx"))
#' nms_pull_matches(luquillo_vft_4quad, c("PY", "PX", "gx", "gy"))
nms_pull_matches <- function(x, .match) {
  stopifnot(is_named(x))
  names(x)[grepl(glue_pipe(anchor(.match)), names(x))]
}
