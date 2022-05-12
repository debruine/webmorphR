#' Face++ Auto-Delineation
#'
#' Automatically delineate faces using dlib in python or Face++ (an external service). Since each delineation counts against a daily limit, you need to set up your own Face++ account (see details below).
#'
#' To use Face++ auto-delineation, you need to get your own free API key from https://www.faceplusplus.com. After signing up for an account, go to https://console.faceplusplus.com/app/apikey/list and request a free API key. Add the key and secret to your .Renviron file as follows:
#'
#' FACEPLUSPLUS_KEY="1234567890abcdefghijk"
#'
#' FACEPLUSPLUS_SECRET="1234567890abcdefghijk"
#'
#' @param stimuli list of class stimlist
#' @param style Which service and number of landmarks (dlib7, fpp106, fpp83)
#' @param replace if FALSE, only gets templates for images with no template
#' @param face which face to delineate in each image if there is more than 1
#'
#' @return stimlist with templates
#' @export
#'
#' @examples
#' \dontrun{
#'   # requires python
#'   auto_high <- demo_stim()[1] %>%
#'     auto_delin(replace = TRUE) # replace existing templates
#'
#'   # requires an API key in .Renviron
#'   auto_low <- demo_stim()[1] %>%
#'     auto_delin(style = "fpp106", replace = TRUE)
#'
#'   c(auto_high, auto_low) %>% draw_tem() %>% plot()
#' }
auto_delin <- function(stimuli, style = c("dlib7", "dlib70", "fpp106", "fpp83"), replace = FALSE, face = 1) {
  stimuli <- validate_stimlist(stimuli)
  style <- match.arg(style)

  if (isTRUE(replace)) stimuli <- remove_tem(stimuli)

  # find out which stimuli need tems
  notems <- sapply(stimuli, `[[`, "points") %>% sapply(is.null)

  if (all(notems == FALSE)) {
    warning("No images needed templates; set replace = TRUE to replace existing templates")
    return(stimuli)
  }
  
  # check for rquired things
  if (style %in% c("fpp106", "fpp83")) {
    ## face++ version
    url <- "https://api-us.faceplusplus.com/facepp/v3/detect"
    key <- Sys.getenv("FACEPLUSPLUS_KEY")
    secret <- Sys.getenv("FACEPLUSPLUS_SECRET")
    if (key == "" || secret == "") {
      stop("You need to set FACEPLUSPLUS_KEY and FACEPLUSPLUS_SECRET in your .Renviron (see ?auto_delin)")
    }
  
    data <- list(
      'api_key' = key,
      'api_secret' = secret,
      'return_landmark' = ifelse(style == "fpp106", 2, 1),
      'image_file' = NULL
    )
  } else if (style %in% c("frl", "dlib70", "dlib7")) {
    if (!reticulate::py_available(TRUE)) {
      stop("You need to install Python to use the dlib templates")
    }
    
    # load script
    pyscript <- system.file("python/facedetect.py", package = "webmorphR")
    pred_file <- paste0("python/", style, ".dat") %>%
      system.file(package = "webmorphR")
    if (pred_file == "") {
      stop("The dlib file could not be found")
    }
    
    reticulate::source_python(pyscript)
  }

  # get line definitions
  fpp <- tem_def(style)
  
  tempdir <- tempfile()
  paths <- stimuli %>%
    remove_tem() %>%
    write_stim(tempdir, format = "jpg", ask = FALSE, overwrite = TRUE) %>%
    unlist() %>%
    setdiff(FALSE) # remove FALSE for unsaved tems

  face <- rep(face, length.out = length(stimuli))

  if (wm_opts("verbose")) {
    pb <- progress::progress_bar$new(
      total = length(stimuli), clear = FALSE,
      format = "Autodelineating [:bar] :current/:total :elapsedfull"
    )
    pb$tick(0)
    Sys.sleep(0.5)
    pb$tick(0)
  }
  
  if (style == "frl") {
    for (i in seq_along(stimuli)) {
      pts <- py_get_points(paths[i], pred_file) %>%
        unlist() %>%
        matrix(nrow = 2, dimnames = list(c("x", "y"), fpp$points$name))
      
      stimuli[[i]]$points <- pts
      
      # replace lines
      stimuli[[i]]$lines <- fpp$lines
      stimuli[[i]]$closed <- fpp$closed
      
      if (wm_opts("verbose")) pb$tick()
    }
  } else if (style %in% c("dlib70", "dlib7")) {
    for (i in seq_along(stimuli)) {
      pts <- py_get_points(paths[i], pred_file) %>%
        unlist() %>%
        matrix(nrow = 2, dimnames = list(c("x", "y"), c()))
      
      # calculate centres of pupils
      if (style == "dlib70") {
        le <- c(38, 39, 41, 42)
        re <- c(44, 45, 47, 48)
      } else if (style == "dlib7") {
        le <- c(3, 4)
        re <- c(1, 2)
      }
      
      left_eye <- pts[, le] %>% apply(1, mean)
      right_eye <- pts[, re] %>% apply(1, mean)
      
      x <- c(left_eye[["x"]], right_eye[["x"]], pts["x", ])
      y <- c(left_eye[["y"]], right_eye[["y"]], pts["y", ])
      
      stimuli[[i]]$points <- matrix(c(x, y), nrow = 2, byrow = TRUE,
                                  dimnames = list(c("x", "y"), fpp$points$name))
      
      # replace lines
      stimuli[[i]]$lines <- fpp$lines
      stimuli[[i]]$closed <- fpp$closed
      
      if (wm_opts("verbose")) pb$tick()
    }
  } else {
    for (i in seq_along(stimuli)) {
      imgname <- names(stimuli)[[i]]
      data$image_file <- paste0(imgname, ".jpg") %>%
        file.path(tempdir, .) %>%
        httr::upload_file()
  
      r <- httr::POST(url, body = data)
      resp <- httr::content(r)
  
      if (!is.null(resp$error_message)) {
        e <- resp$error_message %>%
          paste(collapse = "\n") %>%
          paste0(imgname, ": ", .)
        warning(e, call. = FALSE)
      } else {
        # put in order from fpp
        which_face <- face[i]
        if (length(resp$faces) < which_face) {
          warning(imgname, "  did not have ", which_face, " faces", call. = FALSE)
          which_face <- 1
        }
        alpha_order <- resp$faces[[which_face]]$landmark
        order <- sapply(fpp$points$name, function(x) {
          which(x == names(alpha_order))
        })
        landmarks <- alpha_order[order]
  
        # set up points matrix
        x <- sapply(landmarks, `[[`, "x")
        y <- sapply(landmarks, `[[`, "y")
        dnames <- list(c("x", "y"), names(landmarks))
        stimuli[[i]]$points <- matrix(c(x, y), nrow = 2, byrow = TRUE, dimnames = dnames)
  
        # set up lines from fpp
        stimuli[[i]]$lines <- fpp$lines
        stimuli[[i]]$closed <- fpp$closed
      }
  
      if (wm_opts("verbose")) pb$tick()
    }
  }

  stimuli
}
