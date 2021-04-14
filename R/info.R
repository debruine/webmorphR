#' Add Information
#'
#' Add info with a data table that contains the info in either the same order as the stimulus list, or matching the stimuli item name with the column specified by `.by`.
#'
#' You can also add data as named vectors.
#'
#' @param stimuli list of class stimlist
#' @param ... data table or named vectors of info to add
#' @param .by the column to use to match info to stimuli names; leave NULL if the data are to be matched by order
#'
#' @return stimlist with info added
#' @export
#'
#' @examples
#'
#' stimuli <- demo_stim() %>%
#'   add_info(project = "XXX", gender = c("F", "M"))
#'
#' stimuli$f_multi$info %>% str()
#'
#' info <- data("london_info", package = "webmorphR")
#' stimuli <- demo_stim("london") %>%
#'   add_info(london_info)
#'
#' stimuli$`018_03`$info %>% str()
#'
add_info <- function(stimuli, ..., .by = NULL) {
  stimuli <- validate_stimlist(stimuli)

  # handle table or vector formats
  dots <- list(...)

  if (length(dots) == 1 && is.data.frame(dots[[1]])) {
    # data is in a table
    info <- dots[[1]]
    if (is.null(.by)) {
      .by <- ".by"
      info$.by <- names(stimuli)
    }
  } else {
    # dots are vectors
    if (is.null(.by)) { # add before in case dots are single values
      .by <- ".by"
      dots$.by <- names(stimuli)
    }
    info <- as.data.frame(dots)
  }

  for (nm in names(stimuli)) {
    row <- info[which(info[[.by]] == nm), ]
    row[[.by]] <- NULL
    stimuli[[nm]]$info <- lapply(row, `[`)
  }

  stimuli
}



#' Get Information
#'
#' @param stimuli list of class stimlist
#' @param ... column names to return
#' @param .rownames whether to return a table with no rownames (NULL), rownames from the list item names (NA), or as a new column (the column name as a string)
#'
#' @return a data frame or vector of the info
#' @export
#'
#' @examples
#' stimuli <- demo_stim() %>%
#'   add_info(project = "test", gender = c("F", "M"))
#'
#' get_info(stimuli)
#' get_info(stimuli, "gender")
get_info <- function(stimuli, ..., .rownames = "id") {
  # get all info from stimuli
  info <- lapply(stimuli, `[[`, "info") %>%
    list_to_tbl(rownames = .rownames)
  info$width <- width(stimuli)
  info$height <- height(stimuli)
  info$tem <- lapply(stimuli, `[[`, "points") %>%
    sapply(ncol) %>% sapply(`%||%`, NA)

  # select specified columns
  dots <- c(...)
  if (length(dots) > 1 && is.character(.rownames)) { dots <- c(dots, .rownames) }
  if (length(dots) > 0) {
    info <- info[, dots, drop = FALSE]
    # make vector if dots is 1 item
    if (ncol(info) == 1) {
      info <- info[[1]]
      names(info) <- names(stimuli)
    }
  }

  info
}



#' Image widths
#'
#' @param stimuli list of class stimlist
#' @param type whether to return all widths, min, max, or only unique widths
#'
#' @return vector of widths
#' @export
#'
#' @examples
#'
#' demo_stim("rainbow") %>% width()
width <- function(stimuli, type = c("all", "min", "max", "unique")) {
  stimuli <- validate_stimlist(stimuli)

  w <- sapply(stimuli, `[[`, "width")

  switch(match.arg(type),
         all = w,
         min = min(w, na.rm = T),
         max = max(w, na.rm = T),
         unique = unique(w))
}


#' Image heights
#'
#' @param stimuli list of class stimlist
#' @param type whether to return all heights, min, max, or only unique heights
#'
#' @return vector of heights
#' @export
#'
#' @examples
#'
#' demo_stim("rainbow") %>% height()
height <- function(stimuli, type = c("all", "min", "max", "unique")) {
  stimuli <- validate_stimlist(stimuli)

  h <- sapply(stimuli, `[[`, "height")

  switch(match.arg(type),
         all = h,
         min = min(h, na.rm = T),
         max = max(h, na.rm = T),
         unique = unique(h))
}


#' Remove templates
#'
#' @param stimuli list of class stimlist
#'
#' @return list of class stimlist
#' @export
#'
#' @examples
#' demo_stim() %>% remove_tem()
remove_tem <- function(stimuli) {
  stimuli <- validate_stimlist(stimuli)

  for (i in seq_along(stimuli)) {
    stimuli[[i]]$tempath <- NULL
    stimuli[[i]]$points <- NULL
    stimuli[[i]]$lines <- NULL
    stimuli[[i]]$closed <- NULL
  }

  stimuli
}


#' Check All Templates are the Same
#'
#' @param stimuli list of class stimlist
#'
#' @return logical
#' @export
#'
#' @examples
#' demo_stim() %>% same_tems()
#' c(demo_stim("test"), demo_stim("lisa")) %>% same_tems()
same_tems <- function(stimuli) {
  stimuli <- validate_stimlist(stimuli)

  pts <- lapply(stimuli, `[[`, "points") %>%
    sapply(ncol) %>%
    unique()

  lines <- lapply(stimuli, `[[`, "lines") %>%
    unique()

  if (length(pts) == 1 && length(lines) == 1) {
    TRUE
  } else {
    FALSE
  }
}


#' Get Point Coordinates
#'
#' Get a data frame of the x and y coordinates of a template point
#'
#' @param stimuli list of class stimlist with templates
#' @param pt point(s) to return
#'
#' @return data frame of x and y coordinates of the specified point(s) for each stimulus
#' @export
#'
#' @examples
#' demo_stim() %>% get_point(0:1)
get_point <- function(stimuli, pt = 0) {
  stimuli <- validate_stimlist(stimuli, TRUE)

  pts <- lapply(stimuli, `[[`, "points") %>%
    sapply(`[`, , pt+1) %>%
    t() %>%
    as.data.frame()

  if (ncol(pts) == 2) {
    names(pts) <- c("x", "y")
  } else {
    combo <- expand.grid(coord = c("x", "y"), pt = pt)
    names(pts) <- paste0(combo$coord, combo$pt)
  }

  pts
}
