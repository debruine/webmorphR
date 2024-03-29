#' Apply an oval mask to images
#'
#' Superimpose an oval mask on a set of images. 
#' 
#' @details
#' If the images have templates and `bounds = NULL`, the maxiumum and minimum x and y coordinates for each image will be calculated (or the overall max and min if `each = FALSE`) and an oval with those dimensions and position will be placed over the face.
#'
#' If `bounds` are set to a list of top, right, bottom and left boundaries, these will be used instead of the boundaries derived from templates.
#'
#' @param stimuli list of stimuli
#' @param bounds bounds (t, r, b, l) of oval, calculated from templates if NULL
#' @param fill background color for mask, see [color_conv()]
#' @param each logical; whether to calculate a mask for each image (default) or just one
#'
#' @return list of stimuli with cropped tems and/or images
#' @export
#' @family manipulators
#'
#' @examples
#' # remove external template points and crop
#' stimuli <- demo_stim() |> subset_tem(features("face")) |> crop_tem(25)
#'
#' # three styles of mask
#' omask1 <- mask_oval(stimuli) |> label("default")
#' omask2 <- mask_oval(stimuli, each = FALSE) |> label("each = FALSE")
#' omask3 <- mask_oval(stimuli, bounds = list(t= 50, r = 30, b = 40, l = 30)) |> 
#'   label("manual bounds")
#' 
#' # visualise masks
#' c(omask1, omask2, omask3) |> plot(nrow = 2, byrow = FALSE)
mask_oval <- function(stimuli, bounds = NULL, fill = wm_opts("fill"), each = TRUE) {
  stimuli <- as_stimlist(stimuli) 

  w <- width(stimuli)
  h <- height(stimuli)

  # calculate oval coordinates
  b <- list()
  if (is.null(bounds)) {
    stimuli <- require_tems(stimuli)
    bounds <- bounds(stimuli, each)
    b$top <- bounds$min_y
    b$right <- w - bounds$max_x
    b$bottom <- h - bounds$max_y
    b$left <-  bounds$min_x
  } else {
    bounds <- as.list(bounds)
  }

  borders <- list(
    t = b$top %||% bounds$t %||% bounds[[1]],
    r = b$right %||% bounds$r %||% bounds[[2]],
    b = b$bottom %||% bounds$b %||% bounds[[3]],
    l = b$left %||% bounds$l %||% bounds[[4]]
  )
  
  rx <- (w - borders$r - borders$l)/2
  ry <- (h - borders$t - borders$b)/2
  cx <- borders$l + rx
  cy <- borders$t + ry
  
  fill <- sapply(fill, color_conv)
  suppressWarnings({
    l <- length(stimuli)
    fill <- rep_len(fill, l)
    rx <- rep_len(rx, l)
    ry <- rep_len(ry, l)
    cx <- rep_len(cx, l)
    cy <- rep_len(cy, l)
  })

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
    svg <- sprintf(svg_text, w[i], h[i], 
                   cx[i], cy[i], rx[i], ry[i], 
                   fill[i])

    maskimg <- magick::image_read_svg(svg)
    stimuli[[i]]$img <- magick::image_composite(stimuli[[i]]$img, maskimg)
  }

  stimuli
}
