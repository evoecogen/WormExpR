#' Import Data From Files
#'
#' Returns a list of data from all files in a given folder.
#'
#' @param path `string` Name of path with data files.
#' @param pattern `string` Regular expression for selecting data files.
#' @param ignore_pattern Regular expression for data files to be ignored.
#' @param header `logical` Use headers of tabular data?
#'
#' @importFrom utils read.delim
#' @importFrom magrittr %>% %<>%
#'
#' @return A `list`.
#'
#' @keywords internal
import <-
  function(path, pattern, ignore_pattern = NULL, header) {
    # import cluster data
    temp <- list.files(path = path, pattern = pattern)

    if (!is.null(ignore_pattern)) {
      temp %<>%
        grep(ignore_pattern, ., value = TRUE, invert = TRUE)
    }

    temp %<>%
      file.path(path, .)

    data_list <-
      lapply(temp, function(x)
        read.delim(x, header = header, sep = ","))

    # extract file names and name data_list
    file_names <-
      lapply(temp, function(x)
        sub("\\.[[:alnum:]]+$", "", basename(as.character(x))))
    names(data_list) <- file_names

    return(data_list)
  }

#' Trim Leading And Trailing Spaces
#'
#' @param x `character` Vector, possibly containing leading and trailing spaces.
#'
#' @return A `character` vector.
#'
#' @keywords internal
trim <-
  function (x) {
    gsub("^\\s+|\\s+$", "", x)
  }

#' Test Path Validity
#'
#' Checks whether a given path name actually exists.
#'
#' @param path `string` Name of path to test.
#'
#' @return `TRUE` if `path` is a valid (existing) path name, `FALSE` otherwise.
#'
#' @keywords internal
is_valid_path <-
  function(path) {
    tryCatch({
      normPath <- normalizePath(path, mustWork = TRUE)
      return(TRUE)
    },
    error = function(e) {
      return(FALSE)
    })
  }

#' Test For Integer Value
#'
#' Checks whether a given number has an integer value.
#'
#' @param x `numeric` or `integer` scalar or vector to test.
#'
#' @return `TRUE` if `x` has an integer value, `FALSE` otherwise.
#'
#' @keywords internal
is_integer <-
  function(x) {
    x %% 1 == 0
  }
