#' List format to table format
#'
#' @param list a list of lists
#' @param rownames whether to return a table with no rownames (NULL), rownames from the list item names (NA), or as a new column (the column name as a string)
#'
#' @return a data table
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' list_format <- list(
#'   item1 = list(x = 1, y = "A"),
#'   item2 = list(x = 2),
#'   item3 = list(y = "C", z = TRUE),
#'   item4 = list(x = NULL, y = NULL, z = NULL),
#'   item5 = list(),
#'   item6 = NULL
#' )
#'
#' list_to_tbl(list_format, rownames = "item")
#' }
list_to_tbl <- function(list, rownames = NULL) {
  tbl_format <- list |>
    # handle list() and NULL
    lapply(function(x) {
      if (length(x) == 0) { list(._. = NA) } else { x }
    })  |>
    # handle list(x = NULL)
    lapply(lapply, function(x) {
      if (length(x) == 0) { NA } else { x }
    })  |>
    dplyr::bind_rows()

  tbl_format$._. <- NULL

  if (is.null(rownames)) {
    rownames(tbl_format) <- NULL
  } else if (is.na(rownames)) {
    # no rownames on a tibble :(
    tbl_format <- as.data.frame(tbl_format)
    rownames(tbl_format) <- names(list)
  } else if (is.character(rownames)) {
    tbl_format[[rownames[1]]] <- names(list)
    tbl_format <- dplyr::relocate(tbl_format, !!rownames, .before = 1)
  }

  tbl_format
}

