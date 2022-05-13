#' Apply an oval mask to images
#'
#' Superimpose an oval mask on a set of images. If the images have templates and bounds = NULL, the maxiumum and minimum x and y coordinates for each image will be calculated (or the overall max and min if each = FALSE) and an oval with those dimensions and position will be placed over the face.
#'
#' If bounds are set to a list of top, right, bottom and left boundaries, these will be used instead of the boundaries derived from templates.
#'
#' @param stimuli list of class stimlist
#' @param bounds bounds (t, r, b, l) of oval, calculated from templates if NULL
#' @param fill background color for mask
#' @param each whether to calculate a mask for each image (default) or just one
#'
#' @return stimlist with cropped tems and/or images
#' @export
#'
#' @examples
#' omask1 <- demo_stim() |> mask_oval(fill = "hotpink")
#'
#' # remove external points
#' omask2 <- demo_stim() |>
#'   subset_tem(features("face")) |>
#'   crop_tem(25) |>
#'   mask_oval()
#'
#' # set bounds manually
#' omask3 <- demo_stim() |>
#'   mask_oval(bounds = list(t= 70, r = 120, b = 70, l = 120))
mask_oval <- function(stimuli, bounds = NULL, fill = wm_opts("fill"), each = TRUE) {
  stimuli <- validate_stimlist(stimuli, is.null(bounds))

  n <- length(stimuli)
  w <- width(stimuli)
  h <- height(stimuli)

  b <- list()
  if (is.null(bounds)) {
    bounds <- bounds(stimuli, each)
    b$top <- bounds$min_y
    b$right <- w - bounds$max_x
    b$bottom <- h - bounds$max_y
    b$left <-  bounds$min_x
  } else {
    bounds <- as.list(bounds)
  }

  borders <- list(
    t = b$top %||% bounds$t %||% bounds[1],
    r = b$right %||% bounds$r %||% bounds[2],
    b = b$bottom %||% bounds$b %||% bounds[3],
    l = b$left %||% bounds$l %||% bounds[4]
  )

  rx <- ((w - borders$r - borders$l)/2) |> rep_len(n)
  ry <- ((h - borders$t - borders$b)/2) |> rep_len(n)
  cx <- (borders$l + rx) |> rep_len(n)
  cy <- (borders$t + ry) |> rep_len(n)
  fill <- rep_len(fill, n)

  svg_text <- "<svg
    width=\"%d\" height=\"%d\"
    xmlns=\"http://www.w3.org/2000/svg\">
    <defs><mask id=\"image-mask\">
      <rect fill=\"white\" width=\"100%%\" height=\"100%%\" fill-opacity=\"1\" />
      <ellipse cx='%.1f' cy='%.1f' rx='%.1f' ry='%.1f' />
    </mask></defs>

    <rect width=\"100%%\" height=\"100%%\" fill=\"%s\" mask=\"url(#image-mask)\"/>
    </svg>"

  for (i in seq_along(stimuli)) {
    svg <- sprintf(svg_text, w[i], h[i], cx[i], cy[i], rx[i], ry[i], color_conv(fill[i]))

    maskimg <- magick::image_read_svg(svg)
    stimuli[[i]]$img <- magick::image_composite(stimuli[[i]]$img, maskimg)
  }

  stimuli
}
