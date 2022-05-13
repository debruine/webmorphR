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



#' SVG Path Move
#'
#' @param x,y coordinates
#' @param digits number of digits to round to
#'
#' @return string with M path component
#' @export
#'
#' @examples
#' svgMoveTo(20.123, 30.456)
svgMoveTo <- function(x, y, digits = 2) {
  paste0("M %.", digits, "f %.", digits, "f") |>
    sprintf(round(x, digits), round(y, digits))
}

#' SVG Path Line
#'
#' @param x,y coordinates
#' @param digits number of digits to round to
#'
#' @return string with L path component
#' @export
#'
#' @examples
#' svgLineTo(20.123, 30.456)
svgLineTo <- function(x, y, digits = 2) {
  paste0("L %.", digits, "f %.", digits, "f") |>
    sprintf(round(x, digits), round(y, digits))
}

#' SVG Path Quadratic Curve
#'
#' @param x1,y1 coordinates of control point
#' @param x,y coordinates of main point
#' @param digits number of digits to round to
#'
#' @return string with Q path component
#' @export
#'
#' @examples
#' svgQuadraticTo(20.123, 30.456, 40.123, 50.456)
svgQuadraticTo <- function(x1, y1, x, y, digits = 2) {
  paste0("Q %.", digits, "f %.", digits, "f, %.",
                 digits, "f %.", digits, "f") |>
  sprintf(round(x1, digits), round(y1, digits),
          round(x, digits), round(y, digits))
}

#' SVG Path Quadratic Curve
#'
#' @param x1,y1,x2,y2 coordinates of control points
#' @param x,y coordinates of main point
#' @param digits number of digits to round to
#'
#' @return string with Q path component
#' @export
#'
#' @examples
#' svgCubicTo(20.123, 30.456, 40.123, 50.456, 60.123, 70.456)
svgCubicTo <- function(x1, y1, x2, y2, x, y, digits = 2) {
  paste0("C %.", digits, "f %.", digits, "f, %.",
                 digits, "f %.", digits, "f, %.",
                 digits, "f %.", digits, "f") |>
  sprintf(round(x1, digits), round(y1, digits),
          round(x2, digits), round(y2, digits),
          round(x , digits), round(y , digits))
}

#' Get Control Points
#'
#' @param x0,y0 coordinates of previous point
#' @param x1,y1 coordinates of main point
#' @param x2,y2 coordinates of next point
#' @param t curving factor (0-1)
#'
#' @return x- and y-coordinates of control points (x1, y1, x2, y2)
#' @export
#'
#' @examples
#' svgControlPoints(10, 10, 20, 20, 30, 10)
svgControlPoints <- function(x0, y0, x1, y1, x2, y2, t = 0.3) {
  d01 <- sqrt(`^`(x1-x0,2) + `^`(y1-y0,2))
  d12 <- sqrt(`^`(x2-x1,2) + `^`(y2-y1,2))
  fa <- t*d01/(d01+d12)   # scaling factor for triangle Ta
  fb <- t*d12/(d01+d12)   # ditto for Tb, simplifies to fb=t-fa
  p1x <- x1-fa*(x2-x0)    # x2-x0 is the width of triangle T
  p1y <- y1-fa*(y2-y0)    # y2-y0 is the height of T
  p2x <- x1+fb*(x2-x0)
  p2y <- y1+fb*(y2-y0)

  c(p1x, p1y, p2x, p2y)
}



#' Construct Bezier Curves from Template Points
#'
#' @param v matrix of template points with x and y as rows and point as columns
#' @param idx index of line segment within the path
#'
#' @return string with path component
#' @export
#'
#' @examples
#' stimuli <- demo_stim()
#' # get upper and lower eye points
#' l_upper <- stimuli$f_multi$points[, 18:22]
#' l_lower <- stimuli$f_multi$points[, c(22, 30, 29, 28, 18)]
#' svgBezier(l_upper, 1)
#' svgBezier(l_lower, 2)
svgBezier <- function(v, idx = 1) {
  path <- list()

  if (ncol(v) == 2) {
    # connect with straight line
    if (idx == 1) path[length(path) + 1] = svgMoveTo(v[1, 1], v[2, 1])
    path[length(path) + 1] = svgLineTo(v[1, 2], v[2, 2])
    return(paste(path, collapse = "\n"))
  }

  # connect with bezier curve
  pts <- as.vector(v)
  cp <- c() # control points
  n <- length(pts)
  if (pts[1] == pts[n - 1] && pts[2] == pts[n]) {
    # Draw a closed curve, connected at the ends
    # remove duplicate points and adjust n
    n <- n - 2
    pts <- pts[1:n]

    # Append and prepend knots and control points to close the curve
    pts <- c(pts[(n-1):n], pts, pts[1:4])

    for (j in seq(1, n, 2) ) {
      newPts <- do.call(svgControlPoints, as.list(pts[j:(j+5)]))
      cp <- c(cp, newPts)
    }
    cp <- c(cp, cp[1:2])

    # omit moves in all but first segment in path
    if (idx == 1) path[length(path) + 1] = svgMoveTo(pts[3], pts[4])

    # cubic curves
    for (j in seq(2, n+1, 2)) {
      path[length(path) + 1] = svgCubicTo(
        cp[2 * j - 1], cp[2 * j],
        cp[2 * j + 1], cp[2 * j + 2],
        pts[j + 3], pts[j + 4])
    }
  } else {
    # Draw an open curve, not connected at the ends

    for (j in seq(1, n-4, 2)) {
      newPts <- do.call(svgControlPoints, as.list(pts[j:(j+5)]))
      cp <- c(cp, newPts)
    }

    # omit moves in all but first segment in path
    if (idx == 1) path[length(path) + 1] = svgMoveTo(pts[1], pts[2])

    # first arc
    path[length(path) + 1] = svgQuadraticTo(cp[1], cp[2], pts[3], pts[4])

    # cubic curves
    if (n > 6) {
      for (j in seq(2, n-5, 2)) {
        path[length(path) + 1] = svgCubicTo(
          cp[2 * j - 1], cp[2 * j],
          cp[2 * j + 1], cp[2 * j + 2],
          pts[j + 3], pts[j + 4])
      }
    }

    # last arc
    path[length(path) + 1] = svgQuadraticTo(
      cp[2 * n - 9], cp[2 * n - 8],
      pts[n - 1], pts[n])
  }

  paste(path, collapse = "\n")
}



