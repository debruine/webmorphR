#' SVG Path Move
#'
#' @param x,y coordinates
#' @param digits number of digits to round to
#'
#' @return string with M path component
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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



