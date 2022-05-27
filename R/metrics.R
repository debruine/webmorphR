#' Image shape metrics
#' 
#' Get metrics defined by template points. 
#' 
#' Reference x and y coordinates by point number like `x[0]` or `y[188]`. Use any R functions to process the numbers, as well as `pow()` (same as `^()`, for consistency with webmorph.org). Remember that 0,0 is the top left for images; e.g., `min(y[0], y[1])` gives your the *higher* of the two pupil y-coordinates.
#'
#' @param stimuli list of stimuli with tems
#' @param formula a vector of two points to measure the distance apart, or a string of the formula for the metric
#'
#' @return named vector of the metric
#' @export
#'
#' @examples
#' stimuli <- demo_stim()
#' 
#' metrics(stimuli, c(0, 1)) # eye-spacing
#' 
#' # face width-to-height ratio
#' fwh <- "abs(max(x[113],x[112],x[114])-min(x[110],x[111],x[109]))/abs(y[90]-min(y[20],y[25]))"
#' metrics(stimuli, fwh)
#' 
metrics <- function(stimuli, formula = c(0, 1)) {
  stimuli <- require_tems(stimuli, TRUE)
  
  if (all(is.numeric(formula)) && length(formula) == 2) {
    # distance between two points
    a <- formula[[1]]
    b <- formula[[2]]
    
    formula <- sprintf("sqrt(pow(x[%d]-x[%d], 2) + pow(y[%d]-y[%d],2))", a, b, a, b)
  }
  
  pow <- `^` # webmorph.org uses pow
  rad2deg <- function(degrees) { degrees * (pi/180) }
  
  lapply(stimuli, `[[`, "points") |>
    sapply(function(pt) { 
      x <- pt["x", ]
      y <- pt["y", ]
      
      formula |>
        (\(.) gsub("(x\\[\\d+)\\]", "\\1+1\\]", .))() |>
        (\(.) gsub("(y\\[\\d+)\\]", "\\1+1\\]", .))() |>
        (\(.) parse(text = .))() |> 
        eval()
    })
}
