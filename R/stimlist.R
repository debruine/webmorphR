#' Make a new stimlist
#'
#' @param ... Lists with class "stim"
#' @param .names Names for each stimulus
#'
#' @return A stimlist
#' @export

new_stimlist <- function(..., .names = NULL) {
  stimuli <- list(...)

  check_stim <- lapply(stimuli, class) |>
    sapply(`%in%`, x = "stim") |> all()

  if (!check_stim) {
    stop("All arguments need to have the class 'stim'", call. = FALSE)
  } else {
    class(stimuli) <- c("stimlist", "list")
  }

  if (!is.null(.names)) names(stimuli) <- .names

  stimuli
}

#' Make a new stim
#'
#' @param img Image made by magick
#' @param path Path to image file (or name)
#' @param ... Additional items for stim
#'
#' @return list with class stim
#' @export
#'
new_stim <- function(img, path = "", ...) {
  info <- magick::image_info(img)
  stim_i <- list(
    img = img,
    imgpath = path,
    width = info$width,
    height = info$height
  )
  stim <- c(stim_i, list(...))

  class(stim) <- c("stim", "list")

  stim
}

#' Validate a list of stimuli
#'
#' Check if an object is a list of class stimlist. If class stim, wrap in stimlist. If a properly structured list without the right class, add the right class. If the img is not a magick_image or the pointer is dead, reloads from the imgpath.
#'
#' @param x The object
#' @param tem require templates
#'
#' @return A stimlist
#' @keywords internal
#' @export
#'
validate_stimlist <- function(x, tem = FALSE) {
  # handle list without stim or stimlist classes
  if (is.list(x) &&
      !"stimlist" %in% class(x) &&
      !"stim" %in% class(x)) {

    # does x or the items in x have names consistent with a stim?
    is_stim <- c("points", "lines") %in% names(x) |> all() ||
               c("img", "width", "height") %in% names(x) |> all()
    is_stimlist <- sapply(x, function(xi) {
      c("points", "lines") %in% names(xi) |> all() ||
        c("img", "width", "height") %in% names(xi) |> all()
    }) |> all()

    if (is_stimlist) {
      class(x) <- c("stimlist", "list")
    } else if (is_stim) {
      class(x) <- c("stim", "list")
    }
  }

  # convert stim to stimlist
  if ("stim" %in% class(x)) {
    # convert to stimlist
    stimuli <- list(x)
    class(stimuli) <- c("stimlist", "list")
  } else if ("stimlist" %in% class(x)) {
    stimuli <- x
  } else {
    arg <- match.call()$x
    stop(arg, " needs to be a stimlist")
  }

  # add names
  if (is.null(names(stimuli))) {
    i <- sapply(stimuli, `[[`, "imgpath")
    t <- sapply(stimuli, `[[`, "tempath")
    nm <- mapply(function(i, t) { i %||% t }, i, t)
    names(stimuli) <- unique_names(nm)
  }

  # check if images are available and reload if not
  # TODO: add _magick_magick_image_dead via RCpp
  # for (i in seq_along(stimuli)) {
  #   # TODO: decide if this should warn the user
  #   is_dead <- .Call('_magick_magick_image_dead', PACKAGE = 'magick', stimuli[[i]]$img)
  #   if (is_dead) stimuli[[i]]$img <- magick::image_read(stimuli[[i]]$imgpath)
  # }

  # check if templates are available
  if (tem) {
    no_tems <- lapply(stimuli, `[[`, "points") |> sapply(is.null)
    if (all(no_tems)) {
      stop("No images had templates", call. = FALSE)
    } else if (any(no_tems)) {
      warning("Images without templates were removed: ",
              paste(names(stimuli)[no_tems], collapse = ", "),
              call. = FALSE)
      stimuli <- stimuli[!no_tems]
    }
  }

  stimuli
}
