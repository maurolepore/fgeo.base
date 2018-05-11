#' Create functions to output paths to directories from inst/extdata.
#'
#' This is a function factory that creates other functions. It is intended not
#' for users but for developers. The goal is to quickly and consistently
#' create functions that output paths to directories from inst/data -- where
#' the system stores example data.
#'
#' @param package Character string giving the package name where to look for the
#'   desired data.
#' @param .f A function to apply to a path. Tested functions include `file.path`
#'   , `basename` and `dirname`.
#'
#' @return A string giving the `path` from inst/extdata/ to a file with
#' extension, even if the output of the closure will not be the file itself but
#' the containing directory.
#'
#' @export
#'
#' @examples
#' # Similar to fs::path
#' example_path <- example_path_factory("fgeo.base", file.path)
#' example_path(path = "folder/file.csv")
#'
#' # Similar to fs::path_file
#' example_path_file <- example_path_factory("fgeo.base", basename)
#' example_path_file(path = "folder/file.csv")
#'
#' # Similar to fs::path_dir
#' example_path_dir <- example_path_factory("fgeo.base", dirname)
#' example_path_dir(path = "folder/file.csv")
example_path_factory <- function(package, .f) {
  stopifnot(is.character(package))
  stopifnot(deparse(substitute(.f)) %in% c("file.path", "basename", "dirname"))

    function(path) {
    path_file <- system.file("extdata", path, package = package)
    .f(path_file)
  }
}
