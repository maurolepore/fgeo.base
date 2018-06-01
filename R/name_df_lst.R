# Move to fgeo.base. Replaces fgeo.tool::ls_name_df().

#' Identify each dataframe in a list with a name.
#'
#' Identify each dataframe in a list with the name of the corresponding list
#' item.
#'
#' @param df_lst A list of dataframes.
#' @param name Names of the columns that store the names and values.
#'
#' @return A list of dataframes.
#' @export
#'
#' @examples
#' dfs <- list(a = data.frame(x = 1), b = data.frame(x = 1))
#' name_df_lst(dfs)
#'
#' name_df_lst(dfs, "custom_name")
#'
#' dfs2 <- list(data.frame(x = 1), data.frame(x = 1))
#' name_df_lst(dfs2)
name_df_lst <- function(df_lst, name = "name") {
  check_name_df_lst(df_lst, name)

  df_lst <- fill_names(df_lst, "df")
  lst_nms <- names(df_lst)
  for (i in seq_along(lst_nms)) {
    df_nms <- c(names(df_lst[[i]]), name)
    df_lst[[i]] <- stats::setNames(cbind(df_lst[[i]], lst_nms[[i]]), df_nms)
  }
  df_lst
}

check_name_df_lst <- function(df_lst, name) {
  stopifnot(is.list(df_lst), is.data.frame(df_lst[[1]]), is.character(name))

  any_df_has_cero_row <- any(
    unlist(lapply(df_lst, function(x) nrow(x) == 0 || ncol(x) == 0))
  )
  if (any_df_has_cero_row) {
    stop("All dataframes must have at least one row/column.", call. = TRUE)
  }
}

#' Fill names of an unnamed list
#'
#' @param x A list.
#' @param prefix A prefix for the added names.
#'
#' @examples
#' \dontrun{
#' fill_names(list(1))
#' fill_names(list(1, named_df = 1), "df")
#' }
#' @noRd
fill_names <- function(x, prefix = NULL) {
  stopifnot(is.list(x))
  if (!is.null(prefix)) stopifnot(is.character(prefix))

  filler_names <- paste0(prefix, seq_along(x))
  if (is.null(names(x))) {
    names(x) <- filler_names
  }
  missing_names <- names(x) %in% ""
  if (any(missing_names)) {
    names(x)[missing_names] <- filler_names[missing_names]
  }
  x
}
