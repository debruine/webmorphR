#' Read stimuli
#'
#' @param path Path to directory containing image and/or template files (or a single file path)
#' @param pattern Vector of patterns to use to search for files, or a vector of image indices (e.g., 1:4 selects the first 4 images and their templates if they exist)
#' @param ... further arguments to pass on to other functions, e.g., breaks argument for unique_names()
#'
#' @return a list with class stimlist
#'
#' @export
#'
#' @examples
#' path <- system.file("extdata/test", package = "webmorphR")
#' stimuli <- read_stim(path)
#'
read_stim <- function (path, pattern = NULL, ...) {
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

  i <- grepl(imgext, files, ignore.case = TRUE)
  imgfiles <- files[i]
  t <- grepl("\\.tem$", files, ignore.case = TRUE)
  temfiles <- files[t]


  # process tems ----
  temlist <- lapply(temfiles, function(temfile) {
    # read and clean  ----
    tem_txt <- readLines(temfile, skipNul = TRUE) |> trimws()
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
        sapply(as.integer)

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
      tempath = temfile,
      points = points,
      lines = lines,
      closed = closed
    )
  })

  # load images ----
  imglist <- lapply(imgfiles, function(imgfile) {
      img <- list(
        img = magick::image_read(imgfile),
        imgpath = imgfile
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
  })

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
  args <- list(...)
  breaks <- ifelse(is.null(args$breaks), "/", args$breaks)
  names(stimuli) <- unique_names(df_full$path, breaks)

  class(stimuli) <- c("stimlist", "list")

  stimuli
}

