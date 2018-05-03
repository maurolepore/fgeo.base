#' Guess plot dimensions.
#'
#' @template x_fgeo
#' @param accuracy A number giving the accuracy with which to round `gx` and
#'   `gy`.
#'
#' @return A numeric vector of length 2.
#' @export
#'
#' @examples
#' x <- data.frame(
#'   gx = c(0, 300, 979),
#'   gy = c(0, 300, 481)
#' )
#' guess_plotdim(x)
guess_plotdim <- function(x, accuracy = 20) {
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(accuracy))
  
  names(x) <- tolower(names(x))
  .match <- c("x", "gx", "y", "gy", "x", "px")
  matched <- nms_extract_match(x, .match)
  n_nms <- length(matched)
  if (n_nms != 2) {
    stop("Not enough columns to find x/y positions.\n", matched, call. = FALSE)
  }
  
  guess <- vapply(
    x[ , c("gx", "gy")], guess_max, double(1), accuracy = accuracy
  )
  
  message("Gessing: plotdim = c(", commas(guess), ")")
  unname(guess)
}

#' Guess maximum value of a vector with flexible accuracy.
#'
#' @param x Numeric vector.
#' @param accuracy A single number.
#'
#' @return A number.
#' @export
#'
#' @examples
#' guess_max(1:19, 20)
guess_max <- function(x, accuracy) {
  max_x <- max(x, na.rm = TRUE)
  round_any(max_x, f = ceiling, accuracy = accuracy)
}

#' Extract names that match a character vector.
#'
#' @param x A named object.
#' @param .match A character vector giving names to match.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' nms_extract_match(fgeo.data::luquillo_vft_4quad, c("x", "PX", "gx"))
#' nms_extract_match(fgeo.data::luquillo_stem6_1ha, c("x", "PX", "gx"))
nms_extract_match <- function(x, .match) {
  names(x)[grepl(or(regex_line(.match)), names(x))]
}

#' Add regex line-start and -end.
#'
#' @param x A vector.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' regex_line(c("a", "b"))
regex_line <- function(x) {
  paste0("^", x, "$")
}



#' Round to multiple of any number. Copied from `plyr:::round_any.numeric()`.
#' 
#' @param x Numeric vector to round.
#' @param accuracy Number to round to.
#' @param f Rounding function: floor, ceiling or round.
#' 
#' @seealso `plyr::round_any()` and \url{http://bit.ly/2JrBQK3}.
#' 
#' @export
#' 
#' @examples
#' # From pryr::round_any()
#' round_any(135, 10)
#' round_any(135, 100)
#' round_any(135, 25)
#' round_any(135, 10, floor)
#' round_any(135, 100, floor)
#' round_any(135, 25, floor)
#' round_any(135, 10, ceiling)
#' round_any(135, 100, ceiling)
#' round_any(135, 25, ceiling)
#' @family functions for developers.
#' @family functions to manipulate names.
#' @family functions for developers with no depenciencies.
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}



# Utils 

#' Paste wrappers.
#'
#' @inheritDotParams base::paste
#'
#' @return String.
#' @export
#'
#' @examples
#' or(1:3)
#' @name stick

#' @export
#' @rdname stick 
or <- function(...) {
  paste0(..., collapse = "|")
}

#' @export
#' @rdname stick 
commas <- function(...) {
  paste0(..., collapse = ", ")
}
