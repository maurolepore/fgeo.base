#' Factory of predicate functions to check for multiple values of a variable.
#'
#' Useful in `if()` statements.
#'
#' @param var Character string giving the name of a single variable.
#'
#' @family functions for developers.
#' @family predicates.
#'
#' @return A single `TRUE` or `FALSE`. Insensitive to upper- lower-case.
#'
#' @examples
#' multiple_censusid <- multiple_var("censusid")
#' multiple_censusid(data.frame(CensusID = c(1, 2, NA)))
#' multiple_censusid(data.frame(CensusID = c(1, 1, NA)))
#'
#' # Insensitive to upper/lowercase
#' multiple_censusid <- multiple_var("censusid")
#' multiple_censusid(data.frame(censusid = c(1, 2, NA)))
#' multiple_censusid <- multiple_var("CENSUSID")
#' multiple_censusid(data.frame(censusid = c(1, 2, NA)))
#' @keywords internal
multiple_var <- function(var) {
  force(var)
  var <- tolower(var)
  function(.data) {
    .data <- stats::setNames(.data, tolower(names(.data)))
    .var <- .data[[var]]
    var %in% names(.data) && length(unique(stats::na.omit(.var))) > 1
  }
}
