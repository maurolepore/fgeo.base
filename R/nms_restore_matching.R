#' Rename an object based on case-insensitive match of the names of a reference.
#'
#' @param ref Named object to use as reference.
#' @param new New object which names to restored if they match the reference.
#'
#' @family functions for developers.
#'
#' @return The output is `new` with as many names changed as case-insensitive
#'   matches there are with the reference.
#' @export
#'
#' @examples
#' ref <- data.frame(COL1 = 1, COL2 = 1)
#' new <- data.frame(col1 = 5, col2 = 1, n = 5)
#' nms_restore_matching(new, ref)
nms_restore_matching <- function(new, ref) {
  in_ref <- detect_insensitive(names(new), names(ref))
  names(new)[in_ref] <- extract_insensitive(names(new), names(ref))
  new
}

#' Detect and extract matching strings ignoring case.
#'
#' @param x A string to be muted as in `ref`, it a case insensitive match is
#'   found.
#' @param ref A string to use as a reference to match `x`.
#'
#' @family functions for developers.
#'
#' @return `detect_*` and `extract_*` return a logical vector and a string.
#' @export
#'
#' @examples
#' new <- c("stemid", "n")
#' reference <- c("StemID", "treeID")
#' detect_insensitive(new, reference)
#' extract_insensitive(new, reference)
#'
#' vft <- data.frame(TreeID = 1, Status = 1)
#' extract_insensitive(tolower(names(vft)), names(vft))
#' extract_insensitive(names(vft), tolower(names(vft)))
extract_insensitive <- function(x, ref) {
  ref[detect_insensitive(x, ref)]
}

#' @export
#' @rdname extract_insensitive
detect_insensitive <- function(x, ref) {
  matches <- lapply(fgeo.base::enline(x), grepl, ref, ignore.case = TRUE)
  vapply(matches, any, logical(1))
}
