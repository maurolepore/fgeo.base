#' Drop if missing values.
#'
#' Valuable mostly for its warning.
#'
#' @param dfm A dataframe.
#' @param x String giving a column name of `dfm`.
#'
#' @family functions to filter dataframes.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' dfm <- data.frame(a = 1, b = NA)
#' drop_if_na(dfm, "b")
#' drop_if_na(dfm, "a")
drop_if_na <- function(dfm, x) {
  if (!is.data.frame(dfm)) {
    stop("`dfm` must be a dataframe.", call. = FALSE)
  }
  force(x)

  if (!length(x) == 1) {
    stop("`x` must be of length 1", call. = FALSE)
  }
  if (!x %in% names(dfm)) {
    stop("`x` must be a column name of `dfm`.", call. = FALSE)
  }

  .x <- dfm[[x]]
  missing <- is.na(.x)
  if (any(missing)) {
    warning(
      "Dropping ", sum(missing), " rows with missing `", x, "` values.",
      call. = FALSE
    )
  }
  dfm[!missing, , drop = FALSE]
}
