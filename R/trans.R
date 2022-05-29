#' Transform Images
#' 
#' Transform a base image in shape, color, and/or texture by the differences between two images.
#' 
#' @details 
#' 
#' ### Normalisation options
#' 
#' * none: averages will have all coordinates as the mathematical average of the coordinates in the component templates
#' * twopoint: all images are first aligned to the 2 alignment points designated in `normpoint`. Their position is set to their position in the first image in stimuli
#' * rigid: procrustes aligns all images to the position of the first image in stimuli
#' 
#' ### Sample contours
#' 
#' This interpolates more control points along the lines. This can improve the accuracy of averages and transforms. If you see a “feathery” appearance along lines that have many, close-together points, try turning this off.
#' 
#' ### Warp types
#' 
#' * multiscale: Implements multi-scale affine interpolation for image warping. This is the default, with a good balance between speed and accuracy
#' * linear: Implements triangulated linear interpolation for image warping. Linear warping is least accurate, often resulting in image artifacts, but is very fast.
#' * multiscalerb: Implements multi-scale rigid body interpolation for image warping. This decreases image artifacts in some circumstances, but is much slower.
#'
#' @param trans_img list of stimuli to transform 
#' @param from_img negative transform dimension endpoint (0% image)
#' @param to_img positive transform dimension endpoint (100% image)
#' @param shape,color,texture amount to change along the vector defined by from_img and to_img (can range from -3 to +3)
#' @param outname name to save each image as
#' @param norm how to normalise the images; see Details
#' @param normpoint points for twopoint normalisation
#' @param sample_contours whether to sample contours or just points
#' @param warp warp type
#'
#' @return list of stimuli with transformed images and templates
#' @export
#' @family webmorph
#'
#' @examples
#' \dontrun{
#'   stimuli <- demo_stim()
#'   sexdim <- trans(stimuli, stimuli$f_multi, stimuli$m_multi,
#'                   shape = c(fem = -0.5, masc = 0.5))
#'   
#'   sexdim |> draw_tem() |> label()
#' }
#'
trans <- function(trans_img = NULL, from_img = NULL, to_img = NULL,
                  shape = 0,
                  color = 0,
                  texture = 0,
                  outname = NULL,
                  norm = c("none", "twopoint", "rigid"),
                  normpoint = 0:1,
                  sample_contours = TRUE,
                  warp = c("multiscale", "linear", "multiscalerb")) {
  trans_img <- require_tems(trans_img)
  from_img <- require_tems(from_img)
  to_img <- require_tems(to_img)

  norm <- match.arg(norm)
  warp <- match.arg(warp)
  format <- "jpg" # match.arg(format)
  sample_contours <- ifelse(isTRUE(as.logical(sample_contours)), "true", "false")

  # find identical stimuli to avoid duplicate upload
  to_upload <- c(trans_img, from_img, to_img)

  n <- length(to_upload)
  pairs <- expand.grid(a = 1:n, b = 1:n)
  upairs <- pairs[pairs$a < pairs$b, ]
  idpairs <- mapply(identical, to_upload[upairs$a], to_upload[upairs$b])
  dupes <- upairs[which(idpairs == TRUE), ]
  nondupes <- setdiff(1:n, dupes$b)

  # take care of duplicate names
  dupenames <- duplicated(names(to_upload)[nondupes]) |> which()
  while(length(dupenames) > 0) {
    dnames <- nondupes[dupenames]
    names(to_upload)[dnames] <- paste0(
      names(to_upload[dnames]), "_")
    dupenames <- duplicated(names(to_upload)[nondupes]) |> which()
  }
  from_start <- length(trans_img)+1
  to_start <- length(trans_img)+length(from_img) +1
  names(trans_img) <- names(to_upload)[1:(from_start-1)]
  names(from_img) <- names(to_upload)[from_start:(to_start-1)]
  names(to_img) <- names(to_upload)[to_start:length(to_upload)]

  # save images locally
  tdir <- tempfile()
  files <- write_stim(to_upload[nondupes], tdir, format = "jpg") |> unlist()
  if (length(files) > 200) {
    stop("Sorry! We can't upload more than 100 images at a time. You will need to break your transform into smaller chunks.")
  }
  upload <- lapply(files, httr::upload_file)
  names(upload) <- sprintf("upload[%d]", 0:(length(upload)-1))

  # get all to the same length
  filenames <- list(trans = names(trans_img),
                    from = names(from_img),
                    to = names(to_img))
  n_img <- sapply(filenames, length) |> max()
  filenames <- lapply(filenames, rep_len, n_img) |> as.data.frame()
  filenames$trans <- factor(filenames$trans, unique(filenames$trans))
  filenames$from <- factor(filenames$from, unique(filenames$from))
  filenames$to <- factor(filenames$to, unique(filenames$to))

  n_param <- list(shape, color, texture) |>
    sapply(length) |> max()
  # use indices, not values, so crossing doesn't mangle order
  param <- data.frame(
    shape = rep_len(seq_along(shape), n_param),
    color = rep_len(seq_along(color), n_param),
    texture = rep_len(seq_along(texture), n_param)
  )

  #batch <- tidyr::crossing(param, filenames)
  n <- nrow(filenames)
  batch <- data.frame(
    shape = rep(param$shape, each = n),
    color = rep(param$color, each = n),
    texture = rep(param$texture, each = n),
    trans = rep(filenames$trans, times = n_param),
    from = rep(filenames$from, times = n_param),
    to = rep(filenames$to, times = n_param)
  )
  
  
  # translate back to characters or numbers
  batch$trans <- as.character(batch$trans)
  batch$from <- as.character(batch$from)
  batch$to <- as.character(batch$to)
  batch$shape <- shape[batch$shape]
  batch$color <- color[batch$color]
  batch$texture <- texture[batch$texture]

  # clean params
  for (x in c("shape", "color", "texture")) {
    is_pcnt <- grepl("%", batch[[x]])
    if (is.character(batch[[x]])) 
      batch[x] <- gsub("%", "", batch[[x]]) |> as.numeric()
    # percents are definitely percents
    batch[[x]][is_pcnt] <- batch[[x]][is_pcnt] / 100

    # numbers bigger than 3 are probably percents
    prob_pcnts <- abs(batch[[x]]) > 3
    batch[[x]][prob_pcnts] <- batch[[x]][prob_pcnts] / 100
  }

  # outnames
  if (!is.null(outname)) {
    n <- nrow(batch)
    outname <- gsub("\\.(jpg|gif|png)$", "", outname)
    if (length(outname) < n) {
      outname <- rep_len(outname, n) |>
        paste0("_", 1:n)
    }
    batch$outname <- outname[1:n]
  } else {
    # construct outnames
    trans_names <- names(trans_img)
    from_names <- names(from_img)
    to_names <- names(to_img)
    if (all(from_names == trans_names)) from_names = ""
    if (all(from_names == to_names)) to_names = ""
    if (all(to_names == trans_names)) to_names = ""

    imgnames <- list(trans = trans_names,
                     from = from_names,
                     to = to_names) |>
      lapply(function(x) if (length(x)==1) "" else x) |>
      lapply(rep_len, n_img) |>
      as.data.frame()

    # factorise to keep order
    if (!all(imgnames$trans == "")) {
      imgnames$trans <- factor(imgnames$trans, unique(trans_names))
    }
    if (!all(imgnames$from == "")) {
      imgnames$from <- factor(imgnames$from, unique(from_names))
    }
    if (!all(imgnames$to == "")) {
      imgnames$to <- factor(imgnames$to, unique(to_names))
    }

    paramnames <- data.frame(
      shape = rep_len(names(shape) %||% "", n_param),
      color = rep_len(names(color) %||% "", n_param),
      texture = rep_len(names(texture) %||% "", n_param)
    )

    # fix if no names and multiple params
    if (nrow(paramnames) > 1 &&
        is.null(names(shape)) &&
        is.null(names(color)) &&
        is.null(names(texture))) {
      char_n <- nrow(paramnames) |> as.character() |> nchar()
      paramnames$shape <- paste0("%0", char_n, "d") |>
        sprintf(1:nrow(paramnames))
    }

    if (!all(paramnames$shape == "") && !is.null(names(shape))) {
      paramnames$shape <- factor(paramnames$shape, names(shape))
    }
    if (!all(paramnames$color == "") && !is.null(names(color))) {
      paramnames$color <- factor(paramnames$color, names(color))
    }
    if (!all(paramnames$texture == "") && !is.null(names(texture))) {
      paramnames$texture <- factor(paramnames$texture, names(texture))
    }
    
    # remove exact duplicates
    if (all(paramnames$shape == paramnames$color)) 
      paramnames$color <- ""
    if (all(paramnames$shape == paramnames$texture))
      paramnames$texture <- ""
    if (all(paramnames$color == paramnames$texture))
      paramnames$texture <- ""
    
    pn <- nrow(paramnames)
    imgn <- nrow(imgnames)
    batch$outname <- paste(sep = "_",
      rep(imgnames$trans, times = pn),
      rep(imgnames$from, times = pn),
      rep(imgnames$to, times = pn),
      rep(paramnames$shape, each = imgn),
      rep(paramnames$color, each = imgn),
      rep(paramnames$texture, each = imgn)
    ) |>
      gsub(pattern = "_{2,}", replacement = "_") |>
      gsub(pattern = "^_", replacement = "") |>
      gsub(pattern = "_$", replacement = "")
  }

  # fix missing or duplicate outnames
  if (all(batch$outname == "") ||
      unique(batch$outname) |> length() == 1) {
    batch$outname <- paste0("trans", seq_along(batch$outname))
  }

  # remove image suffixes and make local if starts with /
  batch$outname <- gsub("\\.(jpg|gif|png)$", "", batch$outname)
  batch$outname <- gsub("^/", "./", batch$outname)

  settings <- list(
    count =  nrow(batch)
  )
  for (i in seq_along(batch$outname)) {
    settings[paste0('transimage', i-1)] = batch$trans[[i]]
    settings[paste0('fromimage', i-1)] = batch$from[[i]]
    settings[paste0('toimage', i-1)] = batch$to[[i]]
    settings[paste0('shape', i-1)] = batch$shape[[i]]
    settings[paste0('color', i-1)] = batch$color[[i]]
    settings[paste0('texture', i-1)] = batch$texture[[i]]
    settings[paste0('sampleContours', i-1)] = sample_contours
    settings[paste0('norm', i-1)] = norm
    settings[paste0('warp', i-1)] = warp
    settings[paste0('normPoint0_', i-1)] = normpoint[[1]]
    settings[paste0('normPoint1_', i-1)] = normpoint[[2]]
    settings[paste0('format', i-1)] = format
    settings[paste0('outname', i-1)] = batch$outname[[i]]
  }

  # send request to webmorph and handle zip file
  ziptmp <- paste0(tdir, "/trans.zip")
  httr::timeout(30 + 10*nrow(batch))
  httr::set_config( httr::config( ssl_verifypeer = 0L ) )
  url <- paste0(wm_opts("server"), "/scripts/webmorphR_trans")
  r <- httr::POST(url, body = c(upload, settings) ,
                  httr::write_disk(ziptmp, TRUE))

  utils::unzip(ziptmp, exdir = file.path(tdir, "trans"))

  # check return zip
  nfiles <- file.path(tdir, "trans") |> list.files() |> length()
  if (nfiles == 0) {
    resp <- httr::content(r)
    stop(resp$errorText, call. = FALSE)
  }

  trans <- file.path(tdir, "trans") |>
    read_stim()
  unlink(tdir, recursive = TRUE) # clean up temp directory

  trans[batch$outname]
}
