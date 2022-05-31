#' Get unique names
#'
#' @param full_names a list of full names
#' @param breaks regex for breaking up the names into parts. If "", then each character will be assessed.
#' @param remove_ext whether to remove the extension before comparing
#'
#' @return a list or vector of names with the common beginnings removed
#' @keywords internal
unique_names <- function(full_names,
                         breaks = "/",
                         remove_ext = TRUE) {
  # check is a 1D list or vector
  if (!is.list(full_names) &&
      !is.atomic(full_names)) {
    stop("full_names must be a list or vector")
  } else if (!(sapply(full_names, is.character) |> all())) {
    stop("full_names must contain only character strings")
  }

  # remove extension ----
  fnames <- full_names
  if (remove_ext) {
    fnames <-  gsub("\\..{1,4}$", "", full_names)
  }

  # handle NULL breaks ----
  if (is.null(breaks)) {
    names(fnames) <- full_names
    return(fnames)
  }

  # handle single item ----
  if (length(fnames) == 1) {
    if (breaks == "") {
      unames <- fnames[[1]]
    } else {
      # break and take last section
      unames <- fnames[[1]] |>
        strsplit(breaks) |> # break
        .subset2(1)
      unames <- unames[length(unames)] # get last item
    }
    names(unames) <- full_names[[1]]
    return(unames)
  }

  # compare multiple items ----
  split_names <- fnames |> strsplit(breaks)
  m <- sapply(split_names, length) |> min()

  # check first m sections for overlap ----
  drop_start <- sapply(1:m, function(i) {
    sapply(split_names, `[[`, i) |>
      unique() |>
      length() == 1
  }) |>
    dplyr::cumall() |> # set TRUE until first FALSE
    sum()

  # reverse & check last m sections for overlap ----
  drop_end <- sapply(1:m, function(i) {
    sapply(split_names, function(x) {
      j <- length(x) + 1 - i
      x[[j]]
    }) |>
      unique() |>
      length() == 1
  }) |>
    dplyr::cumall() |>
    sum()

  # trim unvarying characters from each name ----
  is_regex <- grepl("(\\(|\\|)", breaks) # CHECK
  glue <- ifelse(is_regex, "/", breaks)
  unames <- sapply(split_names, function(x) {
    start <- drop_start+1
    stop  <- length(x)-drop_end
    x[start:stop] |> paste(collapse = glue)
  })

  names(unames) <- full_names
  unames
}





