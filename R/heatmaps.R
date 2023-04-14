#' Compare WormExp Query Results
#'
#' This function takes the columns Category, Term and OverlappedID and presents
#' a heatmap that shows the percentage of genes overlapping within all top hits.
#' In addition, the user can provide the name of a specific category to which
#' the comparison should be restricted.
#'
#' Use [`clean_results_table()`] to prepare your WormExp query output for this
#' function. Resulting heatmaps are *NOT SYMMETRIC* and have to be read
#' column-wise.
#'
#' @param input_path `string` Name of path containing cleaned WormExp query
#'   output data files.
#' @param output_path `string` Name of path to store output.
#' @param top `integer` Number of top hits to consider.
#' @param category `string` Name of the category to restrict the comparison to.
#'   `NULL` for no restriction.
#' @param width `integer` Width of png output (in pixels).
#' @param height `integer` Height of png output (in pixels).
#'
#' @seealso [clean_results_table()]
#'
#' @importFrom dplyr arrange distinct slice mutate group_by summarise n
#' @importFrom magrittr %>% %<>%
#' @importFrom tidyr unnest spread
#' @importFrom tibble column_to_rownames
#' @importFrom ComplexHeatmap Heatmap draw column_order
#' @importFrom grDevices dev.off png colorRampPalette
#' @importFrom grid gpar unit
#' @importFrom RColorBrewer brewer.pal
#'
#' @return `NULL` (invisibly; function called for its side effect)
#'
#' @export
#'
#' @examples
#'   \dontrun{
#'   compare_results(
#'     input_path = "WormExp/results",
#'     output_path = "WormExp/figures"
#'   )
#'   }
compare_results <-
  function(input_path,
           output_path,
           top = 30,
           category = NULL,
           width = 1200,
           height = 1200) {
    # Validate parameters
    if (!is_valid_path(input_path) || length(input_path) != 1) {
      stop("'input_path' is not a valid path.", call. = FALSE)
    }

    if (!is_valid_path(output_path) || length(output_path) != 1) {
      stop("'output_path' is not a valid path.", call. = FALSE)
    }

    if (!is_integer(top) || !is.finite(top) || top < 2) {
      stop("'top' must be a positive integer >= 2.", call. = FALSE)
    }

    if (!is.null(category) &&
        (!is.character(category) ||
         length(category) != 1 || !nchar(category))) {
      stop("'category' is not a valid string.", call. = FALSE)
    }

    if (!is_integer(width) || !is.finite(width) || width < 1) {
      stop("'width' must be a positive integer.", call. = FALSE)
    }

    if (!is_integer(height) || !is.finite(height) || height < 1) {
      stop("'height' must be a positive integer.", call. = FALSE)
    }

    # Import data
    data_list <-
      import(path = input_path,
             pattern = "*cleaned.txt",
             header = TRUE)

    # Filter by a category and remove all columns besides Category, Term and Overlapped ID;
    # keep top n results.
    cluster_binary <- list()
    for (i in seq_along(data_list)) {
      cluster_binary[[i]] <- data_list[[i]]

      if (!is.null(category)) {
        cluster_binary[[i]] %<>%
          filter(Category == category)
      }

      cluster_binary[[i]] %<>%
        arrange(FDR, decreasing = FALSE) %>%
        distinct(Term, .keep_all = TRUE) %>%   # sort out duplicates due to data sets in multiple categories
        slice(1:top) %>%
        mutate(OverlappedID = strsplit(as.character(OverlappedID), "[,;]+")) %>%
        unnest(OverlappedID) %>%
        group_by(Term, OverlappedID) %>%
        distinct(OverlappedID, .keep_all = TRUE) %>%
        summarise(n = n()) %>%
        spread(OverlappedID, n, fill = 0) %>%
        column_to_rownames(var = "Term")
    }

    # make matrix out of binary table and switch terms as column (for crossprod)
    cluster_binary <-
      lapply(cluster_binary, function(x)
        t(x))

    # calculate crossproduct -> how much overlap exists between terms and calculate percentages
    cross_prod <-
      lapply(cluster_binary, function(x)
        crossprod(x))
    cross_prod <-
      lapply(cross_prod, function(x)
        floor((x * 100 / diag(x))))

    # name cross_prod objects
    names(cross_prod) <- names(data_list)

    # plot
    title_list <- names(cross_prod)

    # create list with row_order objects
    row_order_list <- list()
    for (i in seq_along(cross_prod)) {
      ht1 <-
        Heatmap(cross_prod[[i]],
                cluster_rows = FALSE,
                cluster_columns = TRUE)
      ht1 <- draw(ht1)
      row_order_list[[i]] <- column_order(ht1)
    }
    dev.off()

    # subset category for correct naming of graphs
    category_sub <- character()

    if (!is.null(category)) {
      category_sub <-
        sub("\\/.*", "", category) %>%
        paste0("_")
    }

    for (i in seq_along(cross_prod)) {
      # save file
      paste0(category_sub, title_list[i], "_Top", top, ".png") %>%
        file.path(output_path, .) %>%
        png(width = width, height = height)

      # column title
      column_title <- paste("Gene Overlap of", title_list[i])

      if (!is.null(category)) {
        column_title <-
          paste("Gene Overlap in category", category, title_list[i])
      }

      # plot file
      ht <-
        Heatmap(
          cross_prod[[i]],
          column_title = column_title,
          column_title_gp = gpar(fontsize = 15),
          col = colorRampPalette(brewer.pal(8, "Reds"))(10),
          cluster_rows = FALSE,
          cluster_columns = TRUE,
          row_order = row_order_list[[i]],
          column_names_gp = gpar(fontsize = 10),
          row_names_gp = gpar(fontsize = 10),
          heatmap_width = unit(18, "cm"),
          heatmap_height = unit(18, "cm"),
          heatmap_legend_param = list(
            title = "Gene Overlap in [%]",
            at = c(0, 20, 40, 60, 80, 100),
            labels = c("0", "20", "40", "60", "80", "100")
          )
        )

      draw(ht, heatmap_legend_side = "left")
      # remove settings
      dev.off()
    }

    invisible()
  }
