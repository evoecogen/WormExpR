#' Clean WormExp Database Reference
#'
#' @param path `string` Name of path where WormExp category files and
#'   WormExp_info are saved.
#' @param ignore_file_names `character` Vector of file names to ignore.
#'
#' @importFrom utils write.table
#' @importFrom readxl read_excel
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @return `NULL` (invisibly; function called for its side effect)
#'
#' @keywords internal
#'
#' @examples
#'   \dontrun{
#'   WormExpR:::clean_reference(
#'     path = "WormExp/GitHub/05_QualityManagement/WormSource_v2.0/"
#'   )
#'   }
clean_reference <-
  function(path,
           ignore_file_names = c("c_elegans.WS283.geneIDs.txt", "reference.txt")) {
    # prepare ignore pattern from ignore_file_names
    ignore_pattern <-
      ignore_file_names %>%
      paste0("^", ., "$") %>%
      gsub("\\.", "\\\\.", .) %>%
      paste(collapse = "|")

    # Import all text files in this folder
    data_list <-
      import(
        path = path,
        pattern = "\\.txt$",
        ignore_pattern = ignore_pattern,
        header = FALSE
      )

    # for all category files, trim gene set names and overwrite file
    category_names <- names(data_list)

    for (i in 1:length(category_names)) {
      temp <- as.data.frame(data_list[i])
      temp <- lapply(temp, trimws)

      paste0(category_names[i], ".txt") %>%
        file.path(path, .) %>%
        write.table(
          temp,
          file = .,
          quote = FALSE,
          row.names = FALSE,
          col.names = FALSE,
          sep = "\t",
          na = "NA"
        )
    }

    # Import references from WormExp_info, trim references and save in reference.txt
    wormexp_info <-
      file.path(path, "WormExp_info.xlsx") %>%
      read_excel(sheet = 2)

    references <-
      wormexp_info %>%
      as.data.frame() %>%
      select(c(Gene_set_name, Refs, Additional_categories))

    trimmed_references <-
      as.data.frame(cbind(
        trim(references$Gene_set_name),
        trim(references$Refs),
        references$Additional_categories
      ))

    file.path(path, "reference.txt") %>%
      write.table(
        trimmed_references,
        file = .,
        sep = "\t",
        col.names = FALSE,
        row.names = FALSE,
        quote = FALSE
      )

    invisible()
  }
