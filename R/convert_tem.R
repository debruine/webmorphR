#' Convert templates
#'
#' @param stimuli list of stimuli
#' @param from id of template definition of stimlist images
#' @param to id of template definition to convert to
#'
#' @return list of stimuli with converted templates
#' @export
#' @keywords internal
convert_tem <- function(stimuli, 
                        from = c("guess", "frl", "fpp106", "fpp83", "dlib70", "dlib7"), 
                        to = c("frl", "fpp106", "fpp83", "dlib70", "dlib7")) {
  stimuli <- require_tems(stimuli)
  
  from <- match.arg(from)
  to <- match.arg(to)
  
  if (from == "guess") {
    pt_n <- lapply(stimuli, `[[`, "points") |> sapply(ncol)
    
    from <- dplyr::case_when(
      pt_n == 189 ~ "frl",
      pt_n == 106 ~ "fpp106",
      pt_n == 83 ~ "fpp83",
      pt_n == 70 ~ "dlib70",
      pt_n == 7 ~ "dlib7",
      TRUE ~ "x"
    )
    
    if (any(from == "x")) {
      notem <- paste(names(stimuli[from == "x"]), collapse = ", ")
      stop("Some images don't have a recognised template: ", notem)
    }
  }
  from <- rep(from, out.length = length(stimuli))
  
  to_tem <- tem_def(to)
  
  pt <- array(c(to_tem$points$x, to_tem$points$y), 
              c(nrow(to_tem$points), 2),
              dimnames = list(
                to_tem$points$name,
                c("x", "y")
              )) |> t()
  
  for (i in seq_along(stimuli)) {
    if (to == from[i]) next # skip if the same
    from_tem <- tem_def(from[i])
    
    stimuli[[i]]$lines <- to_tem$lines
    stimuli[[i]]$closed <- to_tem$closed
    
    # 2-pt align default template to face
    old_points <- stimuli[[i]]$points
    stimuli[[i]]$points <- pt
    pt1 <- from_tem$align_pts[[1]] + 1
    pt2 <- from_tem$align_pts[[2]] + 1
    new_stim <- align(stimuli[i], 
                      pt1 = to_tem$align_pts[[1]], 
                      pt2 = to_tem$align_pts[[2]], 
                      x1 = old_points['x', pt1],
                      x2 = old_points['x', pt2],
                      y1 = old_points['y', pt1],
                      y2 = old_points['y', pt2])
    stimuli[[i]]$points <- new_stim[[1]]$points
    draw_tem(stimuli[[i]], pt.shape = "index", pt.size = 15)
    
    # check for conversion
    x_conversion <- paste0(from[i], ".x") |> tolower()
    y_conversion <- paste0(from[i], ".y") |> tolower()
    
    if (all(c(x_conversion, y_conversion) %in% names(to_tem$points))) {
      from_x   <- as.list(to_tem$points[[x_conversion]]) |>
        lapply(`+`, 1)
      to_x <- which(!sapply(from_x, is.null))
      from_x <- from_x[to_x]
      from_y <- as.list(to_tem$points[[y_conversion]]) |>
        lapply(`+`, 1)
      to_y <- which(!sapply(from_y, is.null))
      from_y <- from_y[to_y]
      
      # handle from_? with multiple points that need to be averaged
      old_x <- sapply(from_x, function(x) {
        mean(old_points['x', x])
      })
      old_y <- sapply(from_y, function(y) {
        mean(old_points['y', y])
      })
      
      stimuli[[i]]$points['x', to_x] <- old_x
      stimuli[[i]]$points['y', to_y] <- old_y
      #draw_tem(stimuli[[i]], pt.shape = "index", pt.size = 15)
    }
  }
  
  stimuli
  }
