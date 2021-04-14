#' Draw template
#'
#' Set alpha to 0 to omit lines or points.
#'
#' @param stimuli list of class stimlist
#' @param pt.color,line.color line or point color
#' @param pt.alpha,line.alpha transparency (0-1), ignored if color is a hex value with transparency
#' @param pt.size,line.size size in pixels (scales to image if NULL)
#' @param pt.shape the shape of the points
#' @param bg background color ("image" uses original image)
#'
#' @return stimlist with template images
#' @export
#'
#' @examples
#' demo_stim() %>% draw_tem() %>% plot()
draw_tem <- function(stimuli, pt.color = wm_opts("pt.color"), pt.alpha = 0.75, pt.size = NULL, pt.shape = c("circle", "cross", "index"),
                     line.color = wm_opts("line.color"), line.alpha = 0.5, line.size = NULL,
                     bg = "image") {
  stimuli <- validate_stimlist(stimuli, TRUE)
  w <- width(stimuli) %>% round()
  h <- height(stimuli) %>% round()
  n <- length(stimuli)
  pt.shape <- match.arg(pt.shape)

  # scale size to image if NULL
  if (is.null(pt.size)) {
    pt.size <- pmax(1, w/100) %>% round(2)
  }
  if (is.null(line.size)) {
    line.size <- pmax(0.5, w/250) %>% round(2)
  }

  # allow for vectors
  pt.color <- rep_len(pt.color, n)
  pt.alpha <- rep_len(pt.alpha, n)
  pt.size <- rep_len(pt.size %||% 0, n)
  line.color <- rep_len(line.color, n)
  line.alpha <- rep_len(line.alpha, n)
  line.size <- rep_len(line.size %||% 0, n)
  bg <- rep_len(bg, n)

  for (i in seq_along(stimuli)) {
    temPoints <- stimuli[[i]]$points
    circle_radius <- max(0.1, pt.size[i]/2 - line.size[i]/2)
    cross_arm <- pt.size[i]/2

    # construct points ----
    idx <- -1
    points <- round(temPoints, 2) %>%
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
          sprintf("<text x=\"%.2f\" y=\"%.2f\">%s</text>", x, y, idx)
        }
      }) %>%
      paste(collapse = "\n          ")

    # construct Bezier curves for lines ----
    if (line.alpha[i] > 0) {
      curves <- stimuli[[i]]$lines %>%
        lapply(function(m) {
            v <- temPoints[, m+1]
            svgBezier(v, 1)
          }) %>%
        lapply(function(d) {
          sprintf("<path d = \"%s\" />",
                  paste(d, collapse = "\n"))
        }) %>%
        paste(collapse = "\n\n")
    } else {
      curves <- ""
    }

    # make SVG ----
    svg <- sprintf("<svg width=\"%d\" height=\"%d\" xmlns=\"http://www.w3.org/2000/svg\">
      <g id=\"lines\" stroke-width=\"%f\" stroke=\"%s\" fill=\"none\">
          %s
      </g>
      <g id=\"points\" stroke-width=\"%f\" stroke=\"%s\" fill=\"%s\" font-size=\"%f\" font-family=\"Helvetica, sans-serif\" font-weight=\"100\" text-anchor=\"middle\" dominant-baseline=\"middle\">
          %s
      </g>
  </svg>",
                   w[i], h[i], line.size[i],
                   color_conv(line.color[i], line.alpha[i]), curves,
                   line.size[i]/2, color_conv(pt.color[i], pt.alpha[i]),
                   color_conv(pt.color[i], pt.alpha[i]), pt.size[i], points)

    temimg <- magick::image_read_svg(svg)

    if (bg[i] == "image") {
      stimuli[[i]]$img <- magick::image_composite(stimuli[[i]]$img, temimg)
    } else if (bg[i] == "none") {
      stimuli[[i]]$img <- temimg
    } else {
      bgcolor <- color_conv(bg[i])
      stimuli[[i]]$img <- magick::image_background(temimg, bgcolor)
    }
  }

  stimuli
}
