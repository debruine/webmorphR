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

#' Convert list to stimlist
#'
#' Checks if an object is a list of class stimlist. If class stim, wrap in stimlist. If a properly structured list without the right class, add the right class. 
#'
#' @param x The object
#' @param tem require templates (images without templates are removed)
#'
#' @return A stimlist
#' @export
#'
as_stimlist <- function(x, tem = FALSE) {
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
  
  # check if stim have width and height and get from img or tem if not
  w <- lapply(stimuli, `[[`, "width")
  h <- lapply(stimuli, `[[`, "height")
  has_dim <- !(sapply(w, is.null) | sapply(h, is.null))
  if (any(!has_dim)) {
    imgs <- lapply(stimuli, `[[`, "img")
    has_img <- !sapply(imgs, is.null)
    pts <- lapply(stimuli, `[[`, "points")
    has_tem <- !sapply(pts, is.null)
    for (i in which(!has_dim)) {
      if (has_img[i]) {
        info <- magick::image_info(imgs[[i]])
        ww <- info$width
        hh <- info$height
      } else if (has_tem[i]) {
        ww <- range(pts[[i]][1, ]) |> sum() |> ceiling()
        hh <- range(pts[[i]][2, ]) |> sum() |> ceiling()
        if (wm_opts("verbose")) {
          sprintf("Stimulus dimensions guessed from template for %s (w = %d, h = %d)", 
                  names(stimuli)[i], ww, hh) |>
            warning(call. = FALSE)
        }
      }
      stimuli[[i]]$width <- ww
      stimuli[[i]]$height <- hh
    }
  }
  
  

  stimuli
}


#' Require templates 
#' 
#' Checks a list of stimuli for templates and omits images without a template. If all_same = TRUE, checks that all the templates are the same type. Errors if no images have a template or not all templates are the same (when all_same == TRUE).
#'
#' @param stimuli list of stimuli
#' @param all_same logical; whether all images should have the same template
#'
#' @return list of stimuli with tems
#' @export
#'
#' @examples
#' stimuli <- demo_stim()
#' have_tems <- require_tems(stimuli)
#' 
#' \dontrun{
#' # produces an error because no tems
#' no_tems <- stimuli |> remove_tem()
#' require_tems(no_tems)
#' 
#' # warns that some images were removed
#' mix_tems <- c(stimuli, no_tems)
#' have_tems <- require_tems(mix_tems)
#' 
#' # produces an error because tems are different
#' demo_tems() |> require_tems(all_same = TRUE)
#' }
require_tems <- function(stimuli, all_same = FALSE) {
  stimuli <- as_stimlist(stimuli)
  
  no_tems <- lapply(stimuli, `[[`, "points") |> sapply(is.null)
  if (all(no_tems)) {
    stop("No images had templates", call. = FALSE)
  } else if (any(no_tems)) {
    if (wm_opts("verbose")) {
      warning("Images without templates were removed: ",
              paste(names(stimuli)[no_tems], collapse = ", "),
              call. = FALSE)
    }
    stimuli <- stimuli[!no_tems]
  }
  
  if (all_same) {
    if (!same_tems(stimuli)) {
      stop("Not all of the images have the same template")
    }
  }
  
  stimuli
}
