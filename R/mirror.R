#' Mirror templates and images
#'
#' Use tem_id to get the symmetry map for your template. If tem_id is omitted, images and templates will be fully reversed (e.g., if point 1 is the left eye in the original image, it will be the right eye in the mirrored image).
#'
#' @param stimuli list of stimuli
#' @param tem_id template ID to be passed to \code{tem_def} (usually "frl" or "fpp106") or NULL
#' @param axis vertical or horizontal axis of mirroring
#'
#' @return list of stimuli with mirrored images and templates
#' @export
#' @family manipulators
#'
#' @examples
#' # load an image and mirror it
#' o <- demo_tems("frl") |> resize(0.5)
#' m <- mirror(o, "frl")
#'
#' # visualise the face outline points
#' c(o, m) |>
#'   subset_tem(features("face")) |>
#'   draw_tem(pt.shape = "index", pt.size = 15) |>
#'   label(c("original", "mirrored"))
#'
mirror <- function(stimuli, tem_id = NULL, axis = "vertical") {
  stimuli <- as_stimlist(stimuli)

  # get symmetry map
  sym_map <- NULL
  if (!is.null(tem_id)) {
    tem <- tem_def(tem_id)
    sym_map <- tem$points$sym
  }

  for (i in seq_along(stimuli)) {
    cx <- (stimuli[[i]]$width-1)/2
    cy <- (stimuli[[i]]$height-1)/2
    p <- stimuli[[i]]$points

    if (axis == "horizontal") {
      stimuli[[i]]$img <- magick::image_flip(stimuli[[i]]$img)
      # flip y points
      if (!is.null(p)) {
        stimuli[[i]]$points <- (p - c(0, cy)) * c(1, -1) +c(0, cy)
      }
    } else {
      stimuli[[i]]$img <- magick::image_flop(stimuli[[i]]$img)
      # flop x points
      if (!is.null(p)) {
        stimuli[[i]]$points <- (p - c(cx, 0)) * c(-1, 1) + c(cx, 0)
      }
    }

    # mirror template points
    if (!is.null(p) && axis == "vertical" && !is.null(sym_map)) {
      n_pt <- ncol(p) - 1 # sym_map is 0-based
      if (all(sym_map %in% 0:n_pt)) {
        stimuli[[i]]$points <- stimuli[[i]]$points[, sym_map+1]
      }
    }
  }

  stimuli
}
