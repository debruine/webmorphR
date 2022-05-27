#' Write tems and images to files
#'
#' @param stimuli list of stimuli
#' @param dir Directory to save to
#' @param names A vector of stimulus names or NULL to use names from the stimuli list
#' @param format output format such as "png", "jpeg", "gif"; is overridden if names end in .png, .jpg, or .gif
#' @param ... other arguments to pass to [magick::image_write], such as quality (for jpegs)
#' @param overwrite whether to overwrite existing files (TRUE/FALSE) or "ask" (only in interactive mode)
#'
#' @return list of saved paths
#' @export
#'
#' @examples
#' \dontrun{
#'   # write demo stim as jpegs to directory ./test_faces
#'   demo_stim() |> write_stim("test_faces", format = "jpg")
#' }
write_stim <- function(stimuli, dir = ".", 
                       names = NULL, format = "png", ..., 
                       overwrite = wm_opts("overwrite")) {
  stimuli <- as_stimlist(stimuli)
  
  if (!is.null(names)) {
    n <- length(stimuli)
    if (length(names) > n) {
      names <- names[1:n]
    } else if (length(names) < n) {
      names <- rep_len(names, n) |> paste0("_", 1:n)
    }
    
    stimuli <- rename_stim(stimuli, names)
  }

  # make dir if it doesn't exist
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  
  # set image format and extension
  format <- gsub("\\.", "", format) |> 
    tolower() |>
    switch(png = "png",
           jpg = "jpeg",
           jpeg = "jpeg",
           gif = "gif",
           "png") # default to png
  
  # iterate over stimuli and names to save 
  # TODO: make this less clunky
  paths <- mapply(function(stim, name) {
    imgsaved <- NULL
    temsaved <- NULL
    
    # save images
    if (!is.null(stim$img)) {
      # get image format from name, if available
      img_format <- format
      has_ext <- grepl("\\.(png|gif|jpg|jpeg)$", tolower(name))
      if (has_ext) {
        img_format <- gsub("^.+\\.", "", tolower(name)) |>
          switch(png = "png",
                 jpg = "jpeg",
                 jpeg = "jpeg",
                 gif = "gif")
        # remove ext from name for tem
        name <- gsub("\\.(png|gif|jpg|jpeg)$", "", 
                     name, ignore.case = TRUE)
      }
      
      ext <- switch(img_format,
                    png = ".png",
                    jpeg = ".jpg",
                    gif = ".gif")
      
      imgpath <- file.path(dir, paste0(name, ext))
      
      # check if file exists
      if (interactive() && overwrite == "ask" && file.exists(imgpath)) {
        txt <- paste0("The file ", imgpath, " already exists; do you want to: \n1: Skip\n2: Save over\n3: Skip all\n4: Save over all")
        ow <- readline_check(txt, 
                             type = "numeric", 
                             min = 1, max = 4)
        
        if (ow == 3) { overwrite <<- FALSE }
        if (ow == 4) { overwrite <<- TRUE }
        
        if (ow == 2 || ow == 4) {
          imgsaved <- magick::image_write(
            stim$img, path = imgpath, format = img_format, ...)
        }
      } else if (isTRUE(overwrite) || !file.exists(imgpath)) {
        imgsaved <- magick::image_write(
          stim$img, path = imgpath, format = img_format, ...)
      }
    }

    # save templates
    if (!is.null(stim$points)) {
      tem_txt <- tem_text(stim)
      tempath <- file.path(dir, paste0(name, ".tem"))
      # check if file exists
      if (interactive() && overwrite == "ask" && file.exists(tempath)) {
        txt <- paste0("The file ", tempath, " already exists; do you want to: \n1: Skip\n2: Save over\n3: Skip all\n4: Save over all")
        ow <- readline_check(txt, 
                             type = "numeric", 
                             min = 1, max = 4)
        
        if (ow == 3) { overwrite <<- FALSE }
        if (ow == 4) { overwrite <<- TRUE }
        
        if (ow == 2 || ow == 4) {
          write(tem_txt, tempath)
          temsaved <- tempath
        }
      } else if (isTRUE(overwrite) || !file.exists(tempath)) {
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


#' Make text version of a template
#'
#' @param stim A list of class stim (one item in a stimlist)
#'
#' @return The text for a .tem file
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' stimuli <- demo_stim()
#' tem_text(stimuli$f_multi)
#' }
tem_text <- function(stim) {
  txt <- list()
  
  # add points
  txt <- c(txt, dim(stim$points)[[2]])
  pts <- apply(stim$points, 2, paste, collapse = "\t")
  txt <- c(txt, pts)
  
  # add lines
  if (!is.null(stim$lines)) {
    txt <- c(txt, length(stim$lines))
    for (i in seq_along(stim$lines)) {
      txt <- c(txt, list(
        as.integer(stim$closed[[i]]),
        length(stim$lines[[i]]),
        paste(stim$lines[[i]], collapse = " ")
      ))
    }
  } else {
    txt <- c(txt, "0")
  }
  
  txt <- paste(txt, collapse = "\n")
  
  txt
}