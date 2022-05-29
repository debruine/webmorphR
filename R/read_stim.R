#' Read stimuli
#' 
#' Read images and templates from a directory.
#'
#' @param path Path to directory containing image and/or template files (or a single file path)
#' @param pattern Vector of patterns to use to search for files, or a vector of image indices (e.g., 1:4 selects the first 4 images and their templates if they exist)
#' @param breaks a vector of characters used to determine the stimulus names from the file names
#'
#' @return a list of stimuli
#' @export
#' @family stim
#'
#' @examples
#' path <- system.file("extdata/test", package = "webmorphR")
#' 
#' # read in all images and templates in a directory
#' stimuli <- read_stim(path)
#' 
#' # read in just images and templates with "m_"
#' m_stimuli <- read_stim(path, "m_")
#'
read_stim <- function (path, pattern = NULL, breaks = "/") {
  imgext <- "\\.(jpg|jpeg|gif|png|bmp)$"
  # get paths to temfiles ----
  if (dir.exists(path) |> all()) {
    if (is.numeric(pattern)) {
      # get images by index and matching tems
      imgpaths <- list.files(path, imgext, full.names = TRUE, ignore.case = TRUE)[pattern]
      tempaths <- sub(imgext, "\\.tem", imgpaths)
      tem_exists <- file.exists(tempaths)
      files <- c(imgpaths, tempaths[tem_exists])
    } else if (is.null(pattern)) {
      files <- list.files(path, full.names = TRUE)
    } else {
      files <- lapply(pattern, list.files, path = path, full.names = TRUE) |> unlist()
    }
  } else if (sapply(path, file.exists) |> all()) {
    files <- path
  } else {
    stop(path, " is neither a directory nor a file")
  }

  # load images ----
  i <- grepl(imgext, files, ignore.case = TRUE)
  imgfiles <- files[i]
  imglist <- lapply(imgfiles, read_img)

  # load tems ----
  t <- grepl("\\.tem$", files, ignore.case = TRUE)
  temfiles <- files[t]
  temlist <- lapply(temfiles, read_tem)

  # join image and tem lists ----
  df_img <- data.frame(
    img_i = seq_along(imgfiles),
    path = tools::file_path_sans_ext(imgfiles)
  )

  df_tem <- data.frame(
    tem_i = seq_along(temfiles),
    path = tools::file_path_sans_ext(temfiles)
  )

  df_full <- dplyr::full_join(df_img, df_tem, by = "path")

  stimuli <- mapply(function(img_i, tem_i) {
    x <- c(imglist[[img_i]], temlist[[tem_i]])
    class(x) <- c("stim", "list")
    x
  }, df_full$img_i, df_full$tem_i, SIMPLIFY = FALSE)

  # assign unique names ----
  names(stimuli) <- unique_names(df_full$path, breaks)

  class(stimuli) <- c("stimlist", "list")

  stimuli
}

#' Read image file
#'
#' @param imgfile 
#'
#' @return list of image info
#' @export
#' @keywords internal
#' @family stim
#'
#' @examples
#' path <- system.file("extdata/test/f_multi.jpg", 
#'                     package = "webmorphR")
#' img <- read_img(path)
read_img <- function(path) {
  img <- list(
    img = magick::image_read(path),
    imgpath = path
  )
  # read attributes
  attr <- magick::image_attributes(img$img)
  wm_desc <- attr$value[attr$property == "exif:ImageDescription"]
  if (length(wm_desc) > 0) {
    img$desc <- tryCatch({
      jsonlite::fromJSON(
        wm_desc,
        simplifyDataFrame = FALSE,
        simplifyVector = TRUE)
    }, error = function(e) { wm_desc })
  }
  # TODO: read embedded tem?
  
  img$img <- magick::image_strip(img$img)
  img_info <- magick::image_info(img$img)
  img$width <- img_info$width
  img$height <- img_info$height
  
  img
}


#' Read tem file
#'
#' @param path path to template file
#'
#' @return list of template info
#' @export
#' @keywords internal
#' @family stim
#'
#' @examples
#' path <- system.file("extdata/test/f_multi.tem", 
#'                     package = "webmorphR")
#' tem <- read_tem(path)
read_tem <- function(path) {
  # read and clean  ----
  tem_txt <- readLines(path, skipNul = TRUE) |> trimws()
  tem_txt <- tem_txt[tem_txt != ""] # get rid of blank lines
  tem_txt <- tem_txt[substr(tem_txt, 1, 1) != "#"] # get rid of comments
  
  # process points ----
  npoints <- as.integer(tem_txt[[1]])
  points <- tem_txt[2:(npoints+1)] |>
    strsplit("\\s+") |>
    sapply(as.numeric)
  dimnames(points) <- list(c("x", "y"), NULL)
  
  # process lines ----
  nlines <- as.integer(tem_txt[[npoints+2]])
  if (nlines > 0) {
    x <- (npoints+3):(npoints+2+(nlines*3))
    line_rows <- tem_txt[x] |>
      matrix(nrow = 3)
    
    lines <- line_rows[3, ] |>
      strsplit("\\s+") |>
      sapply(as.integer, simplify = FALSE)
    
    # TODO: apply webmorph rules for open/closed lines
    closed <- line_rows[1, ] |>
      as.integer() |>
      as.logical()
  } else {
    lines <- c()
    closed <- c()
  }
  
  # create tem object ----
  list(
    tempath = path,
    points = points,
    lines = lines,
    closed = closed
  )
}

