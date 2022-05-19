#' Mask Images with templates
#'
#' Create a mask using template points.
#'
#' For FRL templates, the argument \code{mask} can be a vector with one or more of the following: oval, face, neck, ears (left_ear, right_ear), eyes (left_eye, right_eye), brows (left_brow, right_brow), mouth, teeth, nose.
#'
#' For Face++ templates (fpp83 or fpp106), the argument \code{mask} can be a vector with one or more of the following: face, eyes (left_eye, right_eye), brows (left_brow, right_brow), mouth, teeth, nose. Because these templates have no forehead points, "face" is usually disappointing.
#'
#' Set custom masks using the template points (0-based). View an image with labelled templates using \code{plot(stim, pt.plot = TRUE, pt.shape="index")}. Separate points along a line with commas, line segments with semicolons, and mask areas with colons. For example, this would be the custom mask for the eyes in the fpp83 template:
#'
#' \code{"44,4,56,51,79;79,58,11,25,44:61,67,38,34,40;40,41,17,47,61"}
#'
#' If you set expand = 0, there is sometimes a thin visible line where multiple components of the mask touch.
#'
#' @param stimuli list of class stimlist
#' @param mask vector of masks or a custom list of template points
#' @param fill color to make the mask
#' @param reverse if TRUE, the mask covers the listed areas
#' @param expand how many pixels to expand the mask
#' @param tem_id template ID to pass on to \code{tem_def} to get built-in mask definitions, usually one of "frl", "fpp106" or "fpp83"
#'
#' @return stimlist with masked images
#' @export
#'
#' @examples
#' stimuli <- demo_stim()
#' masked <- mask(stimuli, c("face", "neck", "ears"), "red")
#'
#' revmasked <- mask(stimuli, "eyes", "#FFFF00", TRUE) |>
#'   mask("brows", "purple", TRUE) |>
#'   mask("nose", "#FF000066", TRUE) |>
#'   mask("mouth", "blue", TRUE)
#'
mask <- function(stimuli, mask = "face", fill = wm_opts("fill"),
                 reverse = FALSE, expand = 1, tem_id = "frl") {
  stimuli <- validate_stimlist(stimuli, TRUE)

  # check masks
  if (is.list(mask)) {
    if (is.numeric(unlist(mask))) {
      default_masks <- mask
      names(default_masks) <- paste0("custom_", seq_along(default_masks))
      mask <- names(default_masks)
    } else {
      default_masks <- mask
      mask <- names(default_masks)
    }
  } else if (length(mask) == 1 && grepl("^([0-9]|,|;|:|\\s)+$", mask)) {
    # parse mask
    default_masks <- tryCatch({
      strsplit(mask, "\\s*:\\s*")[[1]] |>
        as.list() |>
        lapply(strsplit, "\\s*;\\s*") |>
        lapply(sapply, strsplit, "\\s*,\\s*") |>
        lapply(sapply, as.integer, simplify = FALSE)
    }, error = function(e) {
      stop("There was a problem parsing the custom mask")
    })
    mask <- paste0("custom_", 1:length(default_masks))
    names(default_masks) <- mask
  } else if (is.character(mask)) {
    tem <- tem_def(tem_id)
    default_masks <- tem$masks

    if ("eyes" %in% mask && !"eyes" %in% names(default_masks)) {
      mask <- c(mask[which(mask != "eyes")], "left_eye", "right_eye")
    }
    if ("ears" %in% mask && !"ears" %in% names(default_masks)) {
      mask <- c(mask[which(mask != "ears")], "left_ear", "right_ear")
    }
    if ("brows" %in% mask && !"brows" %in% names(default_masks)) {
      mask <- c(mask[which(mask != "brows")], "left_brow", "right_brow")
    }

    missing_masks <- setdiff(mask, names(default_masks))
    if (length(missing_masks) > 0) {
      stop("The following masks were not found: ", paste(missing_masks, collapse = ", "))
    }
  } else {
    stop("There was a problem with the mask.")
  }

  # allow for vectors of fill or expand
  n <- length(stimuli)
  fill <- rep_len(fill, n)
  expand <- rep_len(expand, n)
  w <- width(stimuli) |> round()
  h <- height(stimuli) |> round()

  for (i in seq_along(stimuli)) {
    temPoints <- stimuli[[i]]$points

    # construct sets of Bezier curves
    curves <- default_masks[mask] |>
      lapply(function(mm) {
        mapply(function(m, idx) {
          v <- temPoints[, m+1]
          svgBezier(v, idx)
        }, mm, seq_along(mm))
      }) |>
      lapply(function(d) {
        sprintf("<path d = \"%s\" />",
                paste(d, collapse = "\n"))
      }) |>
      paste(collapse = "\n\n")

    # make SVG
    if (isTRUE(reverse)) {
      svg_text <- "<svg
    width=\"%d\" height=\"%d\"
    xmlns=\"http://www.w3.org/2000/svg\">
    <defs><mask id=\"image-mask\" fill=\"white\" stroke=\"white\" stroke-width=\"%f\">
      %s
    </mask></defs>

    <rect width=\"100%%\" height=\"100%%\" fill=\"%s\" mask=\"url(#image-mask)\"/>
</svg>"
    } else {
      svg_text <- "<svg
    width=\"%d\" height=\"%d\"
    xmlns=\"http://www.w3.org/2000/svg\">
    <defs><mask id=\"image-mask\">
    <rect fill=\"white\" width=\"100%%\" height=\"100%%\" fill-opacity=\"1\" />
      <g stroke-width=\"%f\">
      %s
      </g>
    </mask></defs>

    <rect width=\"100%%\" height=\"100%%\" fill=\"%s\" mask=\"url(#image-mask)\"/>
</svg>"

    }

    svg <- sprintf(svg_text, w[i], h[i], expand[i], curves, color_conv(fill[i]))

    maskimg <- magick::image_read_svg(svg)
    stimuli[[i]]$img <- magick::image_composite(stimuli[[i]]$img, maskimg)
  }

  stimuli
}


