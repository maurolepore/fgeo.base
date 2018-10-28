#' Suffix a strings where a vector exactly matches one specific string.
#'
#' @param string A vector of strings to suffix.
#' @param to_match A vector of strings to match with `.match`.
#' @param .match A string to match the vector `to_match` with.
#' @param suffix A suffix to add at the end of each element of `string`.
#'
#' @family general functions to edit data in place
#'
#' @return A modified version of `x`.
#' @export
#'
#' @examples
#' suffix_match(
#'   string = c("one", "banana"),
#'   to_match = c("number", "fruit"),
#'   .match = "number",
#'   suffix = "_num"
#' )
#'
#' suffix_match(
#'   string = c("tag1", "tag2"),
#'   to_match = c("dead", "not-dead"),
#'   .match = "dead",
#'   suffix = ".d"
#' )
#'
#' vft <- data.frame(
#'   Tag = c("01", "02"),
#'   Status = c("dead", "alive"),
#'   stringsAsFactors = FALSE
#' )
#' transform(vft, tagged = suffix_match(Tag, Status, "dead", ".d"))
suffix_match <- function(string, to_match, .match, suffix) {
  check_suffix_match(
    string = string, to_match = to_match, .match = .match, suffix = suffix
  )

  if (!.match %in% to_match) {
    warning(
      "No `string` matches `", .match, "`. Is this what you expect?",
      call. = FALSE
    )
  }

  is_match <- to_match == .match
  string[is_match] <- paste0(string[is_match], suffix)
  string
}

check_suffix_match <- function(string, to_match, .match, suffix) {
  if (!is.character(string)) {
    warning("`string` is not of class character", call. = FALSE)
  }
  not_all_inputs_are_characters <- !all(
    is.character(to_match),
    is.character(.match),
    is.character(suffix)
  )
  if (not_all_inputs_are_characters) {
    stop("Inputs must be characters", call. = FALSE)
  } else {
    invisible(string)
  }
}



#' Create a names-friendly version of a string (lowercase and with no spaces).
#'
#' @param x A character string.
#'
#' @seealso `fgeo.tool::nms_tidy()`.
#' @family general functions to edit data in place
#'
#'
#'
#' @return A modified version of `x`.
#'
#' @export
#' @examples
#' messy <- "Hi yOu"
#'
#' to_tidy_names(messy)
#'
#' messy_named_string <- c(`Messy Name` = messy)
#' # Targets strings, not its names (Target names with `fgeo.tool::nms_tidy()`)
#' to_tidy_names(messy_named_string)
#'
#' dfm <- data.frame(1)
#' setNames(dfm, to_tidy_names(messy))
#'
#' # Makes more sense when operating on strings
#' setNames(list(1), to_tidy_names(messy))
to_tidy_names <- function(x) {
  stopifnot(is.character(x))
  gsub(" ", "_", tolower(x))
}



#' Add regex line-start and -end.
#'
#' @param x A vector.
#'
#' @family general functions to edit data in place
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' anchor(c("a", "b"))
anchor <- function(x) {
  paste0("^", x, "$")
}
