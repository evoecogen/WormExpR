#' Clean WormExp Database ResultsTable
#'
#' This function cleans the WormExp database output and saves it back as .txt
#' file.
#'
#' @param input_file `string` Path to WormExp database output file. Must end in
#'   `".txt"`.
#' @inheritParams compare_results
#'
#' @importFrom readr read_delim write_csv
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange mutate group_by distinct
#' @importFrom tidyr unnest nest
#'
#' @return `NULL` (invisibly; function called for its side effect)
#'
#' @export
#'
#' @examples
#'   \dontrun{
#'   clean_results_table(
#'     input_file = "WormExp/results/ResultsTable.txt",
#'     output_path = "WormExp/results"
#'   )
#'   }
clean_results_table <-
  function(input_file, output_path) {
    # Validate parameters
    if (!is_valid_path(input_file) || length(input_file) != 1) {
      stop("'input_file' is not a valid path.", call. = FALSE)
    }

    if (!is_valid_path(output_path) || length(output_path) != 1) {
      stop("'output_path' is not a valid path.", call. = FALSE)
    }

    results_table <- read_delim(input_file, show_col_types = FALSE)

    # TODO adjust counts
    cleaned_table <- results_table %>%
      arrange(Term) %>%
      mutate(OverlappedID = strsplit(as.character(OverlappedID), "[,;]+")) %>%
      unnest(OverlappedID) %>%
      group_by(Term) %>%
      distinct(OverlappedID, .keep_all = TRUE) %>%
      nest(OverlappedID = c(OverlappedID)) %>%
      mutate(Counts = length(unlist(OverlappedID)),
             OverlappedID = paste(unlist(OverlappedID), collapse = ";"))

    basename(input_file) %>%
      gsub("\\.txt$", "_cleaned.txt", .) %>%
      file.path(output_path, .) %>%
      write_csv(cleaned_table, .)

    invisible()
  }
