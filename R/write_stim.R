#' Write tems and images to files
#'
#' @param stimuli list of class stimlist
#' @param dir Directory to save to
#' @param format output format such as "png", "jpeg", "gif"
#' @param ... other arguments to pass to magick::image_write
#'
#' @return list of saved paths
#' @export
#'
#' @examples
#' \dontrun{
#'   demo_stim() %>% write_stim("test_faces", "jpg")
#' }
write_stim <- function(stimuli, dir = ".", format = "png", ...) {
  stimuli <- validate_stimlist(stimuli)

  # make dir if it doesn't exist
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  paths <- mapply(function(stim, name) {
    # save images
    if (!is.null(stim$img)) {
      format <- switch(tolower(format),
                       png = "png",
                       jpg = "jpeg",
                       jpeg = "jpeg",
                       gif = "gif",
                       "png") # default to png
      ext <- switch(format,
                    png = ".png",
                    jpeg = ".jpg",
                    gif = ".gif")
      imgpath <- file.path(dir, paste0(name, ext))
      magick::image_write(stim$img, path = imgpath, format = format, ...)
    } else {
      imgpath <- NULL
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
      }

      tem_txt <- paste(tem_txt, collapse = "\n")
      tempath <- file.path(dir, paste0(name, ".tem"))
      write(tem_txt, tempath)
    } else {
      tempath <- NULL
    }

    list(tem = tempath,
         img = imgpath)
  }, stimuli, names(stimuli) %||% seq_along(stimuli))

  invisible(paths)
}
