#' Write tems and images to files
#'
#' @param stimuli list of class stimlist
#' @param dir Directory to save to
#' @param names A vector of stimulus names or NULL to use names fro the stimuli list
#' @param format output format such as "png", "jpeg", "gif"
#' @param ... other arguments to pass to magick::image_write
#' @param ask ask if you want to overwrite existing files
#' @param overwrite whether to overwrite existing files
#'
#' @return list of saved paths
#' @export
#'
#' @examples
#' \dontrun{
#'   demo_stim() |> write_stim("test_faces", format = "jpg")
#' }
write_stim <- function(stimuli, dir = ".", names = NULL, format = "png", ..., ask = interactive(), overwrite = !ask) {
  stimuli <- validate_stimlist(stimuli)
  
  if (!is.null(names)) {
    n <- length(stimuli)
    if (length(names) > n) {
      names <- names[1:n]
    } else if (length(names) < n) {
      names <- rep_len(names, n) |> paste0("_", 1:n)
    }
    
    stimuli <- setnames(stimuli, names)
  }

  # make dir if it doesn't exist
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  paths <- mapply(function(stim, name) {
    imgsaved <- NULL
    temsaved <- NULL
    
    # save images
    if (!is.null(stim$img)) {
      format <- gsub("\\.", "", format) |> 
        tolower() |>
        switch(png = "png",
               jpg = "jpeg",
               jpeg = "jpeg",
               gif = "gif",
               "png") # default to png
      
      ext <- switch(format,
                    png = ".png",
                    jpeg = ".jpg",
                    gif = ".gif")
      
      imgpath <- file.path(dir, paste0(name, ext))
      
      # check if file exists
      if (ask && !overwrite && file.exists(imgpath)) {
        txt <- paste0("The file ", imgpath, " already exists; do you want to: \n1: Skip\n2: Save over\n3: Skip all\n4: Save over all")
        ow <- readline_check(txt, 
                             type = "numeric", 
                             min = 1, max = 4)
        
        if (ow == 3) { ask <<- FALSE; overwrite <<- FALSE }
        if (ow == 4) { ask <<- FALSE; overwrite <<- TRUE }
        
        if (ow == 2 || ow == 4) {
          imgsaved <- magick::image_write(
            stim$img, path = imgpath, format = format, ...)
        }
      } else if (overwrite || !file.exists(imgpath)) {
        imgsaved <- magick::image_write(
          stim$img, path = imgpath, format = format, ...)
      }
    }

    # save templates
    if (!is.null(stim$points)) {
      tem_txt <- list()

      # add points
      tem_txt <- c(tem_txt, dim(stim$points)[[2]])
      pts <- apply(stim$points, 2, paste, collapse = "\t")
      tem_txt <- c(tem_txt, pts)

      # add lines
      if (!is.null(stim$lines)) {
        tem_txt <- c(tem_txt, length(stim$lines))
        for (i in seq_along(stim$lines)) {
          tem_txt <- c(tem_txt, list(
            as.integer(stim$closed[[i]]),
            length(stim$lines[[i]]),
            paste(stim$lines[[i]], collapse = " ")
          ))
        }
      } else {
        tem_txt <- c(tem_txt, "0")
      }

      tem_txt <- paste(tem_txt, collapse = "\n")
      tempath <- file.path(dir, paste0(name, ".tem"))
      # check if file exists
      if (ask && !overwrite && file.exists(tempath)) {
        txt <- paste0("The file ", tempath, " already exists; do you want to: \n1: Skip\n2: Save over\n3: Skip all\n4: Save over all")
        ow <- readline_check(txt, 
                             type = "numeric", 
                             min = 1, max = 4)
        
        if (ow == 3) { ask <<- FALSE; overwrite <<- FALSE }
        if (ow == 4) { ask <<- FALSE; overwrite <<- TRUE }
        
        if (ow == 2 || ow == 4) {
          write(tem_txt, tempath)
          temsaved <- tempath
        }
      } else if (overwrite || !file.exists(tempath)) {
        write(tem_txt, tempath)
        temsaved <- tempath
      }
    }

    # return save paths or FALSE if not saved
    list(tem = temsaved,
         img = imgsaved)
  }, stimuli, names(stimuli) %||% seq_along(stimuli))

  invisible(paths)
}
