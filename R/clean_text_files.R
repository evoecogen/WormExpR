#' Remove Duplicates From WormExp Text Files
#'
#' This function cleans the WormExp ResultsTable and saves it back as .txt file
#'
#' @param path `string` Path where to find the WormExp database output.
#' @param output_path `string` Path to write the processed files to.
#'
#' @importFrom readr read_delim
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate case_when filter
#' @importFrom tidyr drop_na separate
#' @importFrom utils read.delim write.table
#'
#' @return `NULL` (invisibly; function called for its side effect)
#'
#' @keywords internal
#'
#' @examples
#'   \dontrun{
#'   WormExpR:::clean_text_files(
#'     path = "WormExp/GitHub/05_QualityManagement/WormSource_v1.5/",
#'     output_path = "WormExp/GitHub/05_QualityManagement/WormSource_v1.5/cleaned/"
#'   )
#'   }
clean_text_files <-
  function(path, output_path) {
    wormexp_info <-
      read_delim(paste(path, "WormExp_info_data.txt", sep = "")) %>%
      select(Additional_categories, Gene_set_name) %>%
      drop_na() %>%
      separate(Additional_categories, c("category_2", "category_3"), sep = ";") %>%
      mutate(
        category_2 = case_when(
          category_2 == "pathogens" ~ 'Pathogen',
          category_2 == "mutants" ~ 'Mutants',
          category_2 == "epigenetics" ~ "Epigenetics",
          category_2 == "daf" ~ "DAF Insulin food",
          category_2 == "development" ~ "Development-Dauer-Aging",
          category_2 == "chemicals" ~ "Chemicalexposure-otherStress",
          category_2 == "tissues" ~ "Tissue-specific",
          category_2 == "targets" ~ "Targets"
        ),
        category_3 = case_when(
          category_3 == "pathogens" ~ 'Pathogen',
          category_3 == "mutants" ~ 'Mutants',
          category_3 == "epigenetics" ~ "Epigenetics",
          category_3 == "daf" ~ "DAF Insulin food",
          category_3 == "development" ~ "Development-Dauer-Aging",
          category_3 == "chemicals" ~ "Chemicalexposure-otherStress",
          category_3 == "tissues" ~ "Tissue-specific",
          category_3 == "targets" ~ "Targets"
        )
      )


    # create a list with gene sets for all categories with gene sets to delete
    categories <-
      c(
        "Tissue-specific",
        "Targets",
        "Chemicalexposure-otherStress",
        "Development-Dauer-Aging",
        "DAF Insulin food",
        "Epigenetics",
        "Mutants",
        "Pathogen"
      )
    categories_data <- list()


    for (i in 1:length(categories)) {
      categories_data[[i]] <- wormexp_info %>%
        filter(category_2 == categories[i] |
                 category_3 == categories[i]) %>%
        select(Gene_set_name)

      names(categories_data)[[i]] <- categories[i]
    }

    # open file fitting to list name and remove all accounts from the respective gene set
    for (i in 1:length(categories_data)) {
      current_file_name <-
        names(categories_data) %>%
        `[[`(i) %>%
        paste0(".txt")

      category_file <-
        file.path(path, current_file_name) %>%
        read.delim(header = FALSE) %>%
        .[!.$V2 %in% unlist(categories_data[[i]]), ]

      file.path(output_path, current_file_name) %>%
        write.table(
          category_file,
          file = .,
          sep = "\t",
          col.names = FALSE,
          row.names = FALSE,
          quote = FALSE
        )
    }

    invisible()
  }
