#' Draw template
#'
#' Visualise a template on an image. 
#' 
#' @details
#' Visualising the index of each point isn't great yet and will overlay 
#'
#' @param stimuli list of stimuli
#' @param pt.color,line.color line or point color, see [color_conv()]
#' @param pt.alpha,line.alpha transparency (0-1), ignored if color is a hex value with transparency. Set alpha to 0 to omit lines or points.
#' @param pt.size,line.size size in pixels (scales to image size if NULL)
#' @param pt.shape the shape of the points ("circle", "cross", "index")
#' @param bg background color ("image" uses the original image)
#'
#' @return list of stimuli with template images
#' @export
#' @family tem
#' @family viz
#'
#' @examples
#' # get an image with 2 different templates
#' stimuli <- demo_tems("frl|fpp106")
#' 
#' # default template
#' draw_tem(stimuli)
#' 
#' \donttest{
#' # custom template
#' draw_tem(stimuli, 
#'          pt.shape = "cross",
#'          pt.color = "red", 
#'          pt.alpha = 1,
#'          pt.size = 15,
#'          line.color = rgb(0, 0, 0),
#'          line.alpha = 0.5,
#'          line.size = 5)
#' 
#' # indexed template
#' draw_tem(stimuli, 
#'          pt.shape = "index",
#'          pt.size = 15, 
#'          pt.alpha = 1,
#'          line.alpha = 0)
#' }
draw_tem <- function(stimuli, pt.color = wm_opts("pt.color"), pt.alpha = 0.75, pt.size = NULL, pt.shape = c("circle", "cross", "index"),
                     line.color = wm_opts("line.color"), line.alpha = 0.5, line.size = NULL,
                     bg = "image") {
  stimuli <- require_tems(stimuli)
  w <- width(stimuli) |> round()
  h <- height(stimuli) |> round()
  pt.shape <- match.arg(pt.shape)

  # scale size to image if NULL
  if (is.null(pt.size)) {
    pt.size <- pmax(1, w/100) |> round(2)
  }
  if (is.null(line.size)) {
    line.size <- pmax(0.5, w/250) |> round(2)
  }

  # allow for vectors
  # pt and line color and alpha combined below
  bg[bg != "image"] <- sapply(bg[bg != "image"], color_conv)
  suppressWarnings({
    l <- length(stimuli)
    pt.color <- rep_len(pt.color, l)
    pt.alpha <- rep_len(pt.alpha, l)
    pt.size <- rep_len(pt.size %||% 0, l)
    line.color <- rep_len(line.color, l)
    line.alpha <- rep_len(line.alpha, l)
    line.size <- rep_len(line.size %||% 0, l)
    bg <- rep_len(bg, l)
  })

  for (i in seq_along(stimuli)) {
    temPoints <- stimuli[[i]]$points
    circle_radius <- max(0.1, pt.size[i]/2 - line.size[i]/2)
    cross_arm <- pt.size[i]/2

    # construct points ----
    idx <- -1
    points <- round(temPoints, 2) |>
      apply(2, function(pts) {
        x <- pts[1]
        y <- pts[2]
        if (pt.shape == "circle") {
          sprintf("<circle cx=\"%.2f\" cy=\"%.2f\" r=\"%.2f\"/>",
                  x, y, circle_radius)
        } else if (pt.shape == "cross") {
          sprintf("<polygon points=\"%.2f,%.2f %.2f,%.2f %.2f,%.2f %.2f,%.2f %.2f,%.2f %.2f,%.2f %.2f,%.2f %.2f,%.2f %.2f,%.2f\" />",
                  x, y, x, y-cross_arm, x, y, x+cross_arm, y,
                  x, y, x, y+cross_arm, x, y, x-cross_arm, y, x, y
          )

          # sprintf("<line x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\" />
          #         <line x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\" />",
          #         x, x, y-cross_arm, y+cross_arm,
          #         x-cross_arm, x+cross_arm, y, y)
        } else if (pt.shape == "index") {
          idx <<- idx + 1 # dumb but works
          sprintf("<text x=\"%.2f\" y=\"%.2f\">%s</text>", x, y+(pt.size/2), idx)
        }
      }) |>
      paste(collapse = "\n          ")

    # construct Bezier curves for lines ----
    if (line.alpha[i] > 0) {
      curves <- svgLines(stimuli[[i]])
    } else {
      curves <- ""
    }

    # make SVG ----
    svg <- sprintf("<svg width=\"%d\" height=\"%d\" xmlns=\"http://www.w3.org/2000/svg\">
      <g id=\"lines\" stroke-width=\"%f\" stroke=\"%s\" fill=\"none\">
          %s
      </g>
      <g id=\"points\" stroke-width=\"%f\" stroke=\"%s\" fill=\"%s\" 
         font-size=\"%f\" font-weight=\"100\" 
         font-family=\"FiraCode, Consolas, Courier, monospace\" 
         text-anchor=\"middle\">
          %s
      </g>
  </svg>",
                   w[i], h[i], line.size[i],
                   color_conv(line.color[i], line.alpha[i]), curves,
                   line.size[i]/2, color_conv(pt.color[i], pt.alpha[i]),
                   color_conv(pt.color[i], pt.alpha[i]), pt.size[i], points)

    temimg <- magick::image_read_svg(svg)

    if (bg[i] == "image") {
      img <- stimuli[[i]]$img
      if (inherits(img, "magick-image")) {
        stimuli[[i]]$img <- magick::image_composite(img, temimg)
      } else {
        stimuli[[i]]$img <- magick::image_background(temimg, wm_opts("fill"))
      }
    } else if (bg[i] == "none") {
      stimuli[[i]]$img <- temimg
    } else {
      bgcolor <- color_conv(bg[i])
      stimuli[[i]]$img <- magick::image_background(temimg, bgcolor)
    }
  }

  stimuli
}
