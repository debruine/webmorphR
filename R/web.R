#' Average Images
#'
#' @param stimuli images to average (list of class stimlist)
#' @param texture logical, textured average
#' @param norm how to normalise
#' @param normpoint points for twopoint normalisation
#'
#' @return stimlist with average image and template
#' @export
#'
#' @examples
#' \dontrun{
#'   avg <- demo_stim() %>% avg()
#' }
avg <- function(stimuli,
                texture = TRUE,
                norm = c("none", "twopoint", "rigid"),
                normpoint = 0:1) {
  stimuli <- validate_stimlist(stimuli, TRUE)
  if (length(stimuli) > 100) {
    stop("We can't average more than 100 images at a time. You can create sub-averages with equal numbers of faces and average those together.")
  }

  norm <- match.arg(norm)
  format <- "jpg" #match.arg(format)

  # save images locally
  tdir <- tempfile()
  files <- write_stim(stimuli, tdir, "jpg") %>% unlist()
  upload <- lapply(files, httr::upload_file)
  names(upload) <- sprintf("upload[%d]", 0:(length(upload)-1))

  settings <- list(
    texture = ifelse(isTRUE(as.logical(texture)), "true", "false"),
    norm = norm,
    normPoint0 = normpoint[[1]],
    normPoint1 = normpoint[[2]],
    format = format
  )

  # send request to webmorph and handle zip file
  ziptmp <- paste0(tdir, "/avg.zip")
  httr::timeout(30 + 10*length(stimuli))
  httr::set_config( httr::config( ssl_verifypeer = 0L ) )
  url <- paste0(wm_opts("server"), "/scripts/webmorphR_avg")
  r <- httr::POST(url, body = c(upload, settings) ,
                  httr::write_disk(ziptmp, TRUE))

  utils::unzip(ziptmp, exdir = paste0(tdir, "/avg"))
  #resp <- httr::content(r)

  avg <- paste0(tdir, "/avg") %>%
    read_stim() %>%
    setnames("avg")
  unlink(tdir, recursive = TRUE) # clean up temp directory
  # avg %>% draw_tem() %>% plot()

  avg
}

#' Continuum
#'
#' @param from_img image to start at
#' @param to_img image to end at
#' @param from starting percentage
#' @param to ending percentage
#' @param by step size
#' @param ... arguments to pass to \code{\link{trans}}
#'
#' @return stimlist
#' @export
#'
#' @examples
#' \dontrun{
#'   stimuli <- demo_stim()
#'   cont <- continuum(stimuli$f_multi, stimuli$m_multi)
#'   animate(cont, 20, rev = TRUE)
#' }
continuum <- function(from_img, to_img, from = 0, to = 1, by = 0.1, ...) {
  steps <- seq(from, to, by)
  trans(from_img, from_img, to_img, steps, steps, steps, ...)
}

#' Loop
#'
#' @param stimuli list of images to morph between
#' @param steps number of steps from one image to the next
#' @param ... arguments to pass to \code{\link{trans}}
#'
#' @return stimlist
#' @export
#'
#' @examples
#' \dontrun{
#'   stimuli <- demo_stim("zoom") %>%
#'     align(procrustes = TRUE) %>%
#'     crop_pad(250, 50, 50, 50) %>%
#'     resize(300)
#'   loop <- loop(stimuli, 5)
#'   animate(loop, 10)
#' }
loop <- function(stimuli, steps = 10, ...) {
  stimuli <- validate_stimlist(stimuli, TRUE)
  if (length(stimuli) < 2) {
    stop("You need at least 2 stimuli")
  }
  if (steps < 2) {
    stop("You need at least 2 steps")
  }

  from_img <- stimuli
  to_img <- c(stimuli[2:length(stimuli)], stimuli[1])
  p <- seq(0, 1, length.out = steps+1)
  p <- p[1:(length(p)-1)]
  loop <- trans(from_img, from_img, to_img, p, p, p)

  # get the right order
  order <- paste0(names(from_img), "_", names(to_img)) %>%
    lapply(grepl, names(loop)) %>%
    lapply(which) %>%
    unlist()

  loop[order]
}


#' Transform Images
#'
#' @param trans_img image(s) to transform (list of class stimlist)
#' @param from_img negative transform dimension endpoint (0% image)
#' @param to_img positive transform dimension endpoint (100% image)
#' @param shape,color,texture amount to change along the vector defined by from_img and to_img (can range from -3 to +3)
#' @param outname name to save each image as
#' @param norm how to normalise the images
#' @param normpoint points for twopoint normalisation
#' @param sample_contours whether to sample contours or just points
#' @param warp warp type
#'
#' @return stimlist with transformed images and templates
#' @export
#'
#' @examples
#' \dontrun{
#'   stimuli <- demo_stim()
#'   sexdim <- trans(stimuli, stimuli$f_multi, stimuli$m_multi,
#'                   shape = c(fem = -0.5, masc = 0.5))
#'   sexdim %>% draw_tem() %>% label() %>% plot()
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
  trans_img <- validate_stimlist(trans_img, TRUE)
  from_img <- validate_stimlist(from_img, TRUE)
  to_img <- validate_stimlist(to_img, TRUE)

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
  dupenames <- duplicated(names(to_upload)[nondupes]) %>% which()
  while(length(dupenames) > 0) {
    dnames <- nondupes[dupenames]
    names(to_upload)[dnames] <- paste0(
      names(to_upload[dnames]), "_")
    dupenames <- duplicated(names(to_upload)[nondupes]) %>% which()
  }
  from_start <- length(trans_img)+1
  to_start <- length(trans_img)+length(from_img) +1
  names(trans_img) <- names(to_upload)[1:(from_start-1)]
  names(from_img) <- names(to_upload)[from_start:(to_start-1)]
  names(to_img) <- names(to_upload)[to_start:length(to_upload)]

  # save images locally
  tdir <- tempfile()
  files <- write_stim(to_upload[nondupes], tdir, "jpg") %>% unlist()
  if (length(files) > 20) {
    stop("Sorry! We can't upload more than 100 images at a time. You will need to break your transform into smaller chunks.")
  }
  upload <- lapply(files, httr::upload_file)
  names(upload) <- sprintf("upload[%d]", 0:(length(upload)-1))

  # get all to the same length
  filenames <- list(trans = names(trans_img),
                    from = names(from_img),
                    to = names(to_img))
  n_img <- sapply(filenames, length) %>% max()
  filenames <- lapply(filenames, rep_len, n_img) %>% as.data.frame()

  n_param <- list(shape, color, texture) %>%
    sapply(length) %>% max()
  param <- data.frame(
    shape = rep_len(shape, n_param),
    color = rep_len(color, n_param),
    texture = rep_len(texture, n_param)
  )

  batch <- tidyr::crossing(param, filenames)

  # clean params
  for (x in c("shape", "color", "texture")) {
    is_pcnt <- grepl("%", batch[[x]])
    if (is.character(batch[[x]])) batch[x] <- gsub("%", "", batch[[x]]) %>% as.numeric()
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
      outname <- rep_len(outname, n) %>%
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
                     to = to_names) %>%
      lapply(function(x) if (length(x)==1) "" else x) %>%
      lapply(rep_len, n_img) %>%
      as.data.frame()

    # factorise to keep order
    if (!all(imgnames$trans == "")) {
      imgnames$trans <- factor(imgnames$trans, trans_names)
    }
    if (!all(imgnames$from == "")) {
      imgnames$from <- factor(imgnames$from, from_names)
    }
    if (!all(imgnames$to == "")) {
      imgnames$to <- factor(imgnames$to, to_names)
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
      paramnames$shape <- nrow(paramnames) %>%
        as.character() %>%
        nchar() %>%
        paste0("%0", ., "d") %>%
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

    o <- tidyr::crossing(paramnames, imgnames)
    batch$outname <- paste(o$trans, o$from, o$to,
                           o$shape, o$color, o$texture,
                           sep = "_") %>%
      gsub("_{2,}", "_", .) %>%
      gsub("^_", "", .) %>%
      gsub("_$", "", .)
  }

  # fix missing or duplicate outnames
  if (all(batch$outname == "") ||
      unique(batch$outname) %>% length() == 1) {
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

  utils::unzip(ziptmp, exdir = paste0(tdir, "/trans"))

  # check return zip
  nfiles <- paste0(tdir, "/trans") %>% list.files() %>% length()
  if (nfiles == 0) {
    resp <- httr::content(r)
    stop(resp$errorText, call. = FALSE)
  }

  trans <- paste0(tdir, "/trans") %>%
    read_stim()
  unlink(tdir, recursive = TRUE) # clean up temp directory
  # trans %>% draw_tem() %>% plot()

  trans
}


#' Symmetrise Images
#'
#' @param stimuli images to symmatrise (list of class stimlist)
#' @param shape,color amount of symmetry (0 for none, 1.0 for perfect)
#' @param tem_id template ID to be passed to \code{\link{tem_def}} (usually "frl" or "fpp106")
#' @param ... Additional arguments to pass to \code{\link{trans}}
#'
#' @return stimlist with symmatrised images and templates
#' @export
#'
#' @examples
#' \dontrun{
#'   stimuli <- demo_stim("london", 1) %>% resize(0.5)
#'
#'   sym_both <- symmetrize(stimuli)
#'   sym_shape <- symmetrize(stimuli, color = 0)
#'   sym_color <- symmetrize(stimuli, shape = 0)
#'   sym_anti <- symmetrize(stimuli, shape = -1.0, color = 0)
#' }
symmetrize <- function(stimuli, shape = 1.0, color = 1.0, tem_id = "FRL", ...) {
  stimuli <- validate_stimlist(stimuli, TRUE)

  mirror <- mirror(stimuli, tem_id) %>%
    setnames(prefix = "mirror_")

  sym <- trans(trans_img = stimuli, from_img = stimuli, to_img = mirror,
               shape = shape[[1]]/2, color = color[[1]]/2, texture = color[[1]]/2,
               outname = names(stimuli), ...)

  sym
}
