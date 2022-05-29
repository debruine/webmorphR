#' Get template definition
#'
#' Template definitions are lists that contain information about templates that are needed to do things like symmetrising and masking images. This function is mostly used internally.
#' 
#' @details 
#' If you have defined a custom template on webmorph.org, you can get its function definition by ID. You can see the ID numbers next to the templates available to you under the *Template > Current Template* menu. 
#'
#' @param tem_id the name of a built-in template (frl, fpp106, fpp83, dlib70, or dlib7) or a numeric ID of a template to retrieve from webmorph.org
#' @param path path of local tem definition file
#'
#' @return list with template definition
#' @export
#' @family tem
#'
#' @examples
#' fpp106 <- tem_def("fpp106")
#' fpp106$lines |> str()
#'
#' \donttest{
#' fpp83 <- tem_def("fpp83")
#' fpp83$mask |> str()
#'
#' frl <- tem_def("frl")
#' frl$points[1:10, ]
#' viz_tem_def(frl, pt.size = 10, line.size = 5)
#' }
tem_def <- function(tem_id = "frl", path = NULL) {

  # read file or url ----
  if (!is.null(path)) {
    if (!file.exists(path)) {
      stop(sprintf("The file at %s does not exist", path))
    }
    tem_def <- tryCatch({
      jsonlite::read_json(path, simplifyVector = TRUE,
                          simplifyMatrix = FALSE)
    }, error = function(e) {
      stop("The file couldn't be read")
    })
  } else if (is.numeric(tem_id)) {
    url <- sprintf("https://webmorph.org/scripts/temDownloadJSON?tem_id=%d",
                   tem_id)
    tem_def <- tryCatch({
      jsonlite::read_json(url, simplifyVector = TRUE,
                          simplifyMatrix = FALSE)
    }, error = function(e) {
      stop("You might not have an internet connection")
    })
  } else if (is.character(tem_id)) {
    temdir <- system.file("extdata/tem_defs", package = "webmorphR")
    temdefs <- list.files(temdir, "\\.json$", full.names = TRUE)
    match <- grepl(tolower(tem_id), 
                   tolower(temdefs), 
                   fixed = TRUE)
    if (all(match == FALSE)) {
      stop(tem_id, " is not a built-in template")
    }

    tem_def <- jsonlite::read_json(temdefs[match][[1]],
                                   simplifyVector = TRUE,
                                   simplifyMatrix = FALSE)
  } else {
    stop("You must supply a numeric tem_id or a valid path to a template definition file.")
  }

  tem_def
}

#' Visualise a template definition
#'
#' @param tem_def the template definition; usually from [tem_def()]
#' @param ... further arguments to pass to [draw_tem()]; pt.size and line.size often need to be adjusted
#'
#' @return a stimlist with a blank image and the template drawn on it
#' @export
#' @family tem
#'
#' @examples
#' dlib70 <- tem_def("dlib70")
#' viz_tem_def(dlib70, pt.size = 5, line.size = 3)
#' 
#' \dontrun{
#' # get the FRL-bodies template from webmorph.org
#' # see https://osf.io/g27wf/ for open-access body images with this template
#' # warning: they are all nude (paid models from 3d.sk)
#' frl_bodies <- tem_def(4)
#' viz_tem_def(frl_bodies)
#' }
viz_tem_def <- function(tem_def, ...) {
  # make a blank image the size of the template
  width <- tem_def$width %||% mean(tem_def$points$x) * 2
  height <- tem_def$height %||% max(tem_def$points$y) + 20
  x <- blank(1, width, height) 
  
  # add the default template points and line to the blank image
  points <- tem_def$points[c("x", "y")] |> 
    as.matrix() |> t()
  
  colnames(points) <- tem_def$points$name
  
  x[[1]]$points <- points
  x[[1]]$lines <- tem_def$lines
  x[[1]]$closed <- tem_def$closed
  
  draw_tem(x, ...)
}


#' Change template lines
#' 
#' Alter, add or remove lines in a template
#'
#' @param stimuli list of stimuli
#' @param line_id index of the line to change
#' @param pts vector of points to change the line_idx to (deletes line if NULL)
#'
#' @return stimlist with altered templates
#' @export
#' @family tem
#'
#' @examples
#' # get image with dlib70 template and view lines
#' s <- demo_tems("dlib70")
#' s[[1]]$lines
#' 
#' # remove all lines
#' s2 <- change_lines(s, line_id = 1:13, pts = NULL)
#' s2[[1]]$lines
#' 
#' # visualise point indices
#' draw_tem(s2, pt.shape = "index", pt.size = 15)
#' 
#' # add a new line
#' s3 <- change_lines(s2, line_id = "face_outline", 
#'                    pts = c(2:18, 28:19, 2))
#' s3[[1]]$lines
#' draw_tem(s3)
change_lines <- function(stimuli, line_id = 1, pts = NULL) {
  for (i in seq_along(stimuli)) {
    oldlines <- stimuli[[i]]$lines
    if (is.null(pts)) {
      oldlines[line_id] <- NULL
    } else {
      oldlines[[line_id]] <- pts
    }
    stimuli[[i]]$lines <- oldlines
  }
  
  stimuli
}

#' Subset template points
#' 
#' Keep or delete specified template points. Points will be renumbered and line definitions will be updated. If all points in a line are deleted, the line will be removed. POint indexing is 0-based, so the first two points (usually the pupils) are 0 and 1.
#' 
#' @param stimuli list of stimuli
#' @param ... vectors of points to keep or delete
#' @param keep logical; whether to keep or delete the points
#'
#' @return stimlist with altered templates
#' @export
#' @family tem
#'
#' @examples
#' # keep just the first two points
#' demo_stim(1) |>
#'   subset_tem(0:1) |>
#'   draw_tem(pt.size = 10)
#' 
#' # remove the last 10 points 
#' # (produces the 179-point Perception Lab template)
#' demo_stim(1) |>
#'   subset_tem(179:188, keep = FALSE) |>
#'   draw_tem()
#' 
#' # use features() to keep only points from a pre-defined set 
#' # "gmm" is points used for geometric morphometrics
#' demo_stim(1) |>
#'   subset_tem(features("gmm")) |>
#'   draw_tem()
#'
subset_tem <- function(stimuli, ..., keep = TRUE) {
  stimuli <- require_tems(stimuli, TRUE)
  points <- list(...) |> unlist() |> unique() |> sort()
  
  for (i in seq_along(stimuli)) {
    oldpts <- stimuli[[i]]$points
    oldlines <- stimuli[[i]]$lines
    oldclosed <- stimuli[[i]]$closed
    
    full_idx <- 1:dim(oldpts)[[2]]
    
    # remove points, webmorph points are 0-indexed so +1
    if (keep) {
      keep_idx <- points + 1
    } else {
      keep_idx <- setdiff(full_idx, points+1)
    }
    
    # check for bad points
    if (setdiff(keep_idx, full_idx) |> length() > 0) {
      stop("Some points are not in the template")
    }
    
    newpoints <- oldpts[, keep_idx]
    
    # translate
    trans <- sapply(full_idx, function(x) {
      ifelse(x %in% keep_idx, match(x, keep_idx), NA)
    })
    
    # remove points from lines and renumber
    newlines <- lapply(oldlines, function(x) {
      # translate tem idx to r idx
      l <- trans[(x + 1)] |>
        stats::na.omit() |>
        as.vector()
      if (length(l) > 1) {
        # only return if >1 points remain
        return(l - 1) # translate r idx to tem idx
      }
    })
    
    removed_lines <- sapply(newlines, is.null)
    newlines <- newlines[!removed_lines]
    newclosed <- oldclosed[!removed_lines]
    
    stimuli[[i]]$points <- newpoints
    stimuli[[i]]$lines <- newlines
    stimuli[[i]]$closed <- newclosed
  }
  
  stimuli
}

#' Feature Points
#' 
#' Get point indices for features, usually for use with \code{\link{subset_tem}}.
#' 
#' Available features for the frl template are: "gmm", "oval", "face", "mouth", "nose", "eyes", "brows", "left_eye", "right_eye", "left_brow",  "right_brow", "ears", "undereyes", "teeth", "smile_lines", "cheekbones", "philtrum", "chin", "neck", "halo".
#' 
#' Available features for the dlib70 template are: "teeth", "left_eye", "right_eye", "left_brow", "right_brow", "nose", "mouth", "face".
#'
#' @param ... a vector of feature names (see Details)
#' @param tem_id template ID (currently only works for frl and dlib70)
#'
#' @return vector of corresponding template indices
#' @export
#' @family tem
#'
#' @examples
#' features("mouth")
#' features("gmm")
#' features("nose", tem_id = "dlib70")
#'
features <- function(..., tem_id = c("frl", "dlib70")) {
  # 0-based for compatibility with webmorph
  tem_id <- match.arg(tem_id)
  
  named_features <- list(...) |> unlist()
  
  if (tem_id == "frl") {
    features <- list(
      # imprecise
      undereyes = c(44:49),
      ears = c(115:124),
      halo = c(146:156),
      teeth = c(99:103),
      smile_lines = c(158:163),
      cheekbones = c(164:169),
      philtrum = c(170:173),
      chin = c(174:178),
      neck = c(183:184, 145, 157),
      # features
      left_eye = c(0, 2:9, 18:22, 28:30, 34:38),
      right_eye = c(1, 10:17, 23:27, 31:33, 39:43),
      left_brow = c(71:76, 83:84),
      right_brow = c(77:82, 85:86),
      nose = c(50:70, 170, 172, 179:182),
      mouth = c(87:108),
      face = c(109:114, 125:144, 185:188)
    )
    
    features$oval <- c(features$halo, features$neck)
  } else if (tem_id == "dlib70") {
    features <- list(
      # imprecise
      teeth = c(62:69),
      # features
      left_eye = c(0, 38:43),
      right_eye = c(1, 44:49),
      left_brow = c(19:23),
      right_brow = c(24:28),
      nose = c(29:37),
      mouth = c(50:69),
      face = c(2:18)
    )
  }
  
  # combo features
  features$eyes <- c(features$left_eye, features$right_eye)
  features$brows <- c(features$left_brow, features$right_brow)
  features$gmm <- c(features$face, features$eyes, features$brows, 
                    features$nose, features$mouth)
  features$all <- 0:max(unlist(features))
  
  # unavailable and duplicate features are ignored
  features[named_features] |>
    unlist() |> unname() |>
    unique() |> sort()
}



#' Remove templates
#'
#' @param stimuli list of stimuli
#'
#' @return list of stimuli
#' @export
#' @family tem
#'
#' @examples
#' demo_stim() |> remove_tem()
remove_tem <- function(stimuli) {
  stimuli <- as_stimlist(stimuli)
  
  for (i in seq_along(stimuli)) {
    stimuli[[i]]$tempath <- NULL
    stimuli[[i]]$points <- NULL
    stimuli[[i]]$lines <- NULL
    stimuli[[i]]$closed <- NULL
  }
  
  stimuli
}


#' Check All Templates are the Same
#'
#' @param stimuli list of stimuli
#'
#' @return logical
#' @export
#' @family tem
#'
#' @examples
#' stim <- demo_stim()
#' stim2 <- subset_tem(stim, features("gmm"))
#' 
#' same_tems(stim)
#' 
#' c(stim, stim2) |> same_tems()
same_tems <- function(stimuli) {
  stimuli <- as_stimlist(stimuli)
  
  pts <- lapply(stimuli, `[[`, "points") |>
    sapply(ncol) |>
    unique()
  
  lines <- lapply(stimuli, `[[`, "lines") |>
    unique()
  
  if (length(pts) == 1 && length(lines) == 1) {
    TRUE
  } else {
    FALSE
  }
}


#' Get Point Coordinates
#'
#' Get a data frame of the x and y coordinates of a template point
#'
#' @param stimuli list of stimuli with templates
#' @param pt point(s) to return
#'
#' @return data frame of x and y coordinates of the specified point(s) for each stimulus
#' @export
#' @family tem
#' @family info
#'
#' @examples
#' demo_stim() |> get_point(0:1)
get_point <- function(stimuli, pt = 0) {
  stimuli <- require_tems(stimuli)
  
  all_pts <- tems_to_array(stimuli)
  # this function reverses y-coordinates
  
  data.frame(
    image = rep(names(stimuli), each = length(pt)),
    point = rep(pt, times = length(stimuli)),
    x = all_pts[pt+1, "X", ] |> matrix(ncol = 1),
    y = all_pts[pt+1, "Y", ] |> matrix(ncol = 1) * -1
  )
}

#' Get center coordinates
#'
#' @param stimuli list of stimuli
#' @param points which points to include (0-based); if NULL, all points will be used
#'
#' @return named matrix of centroid x and y coordinates
#' @export
#' @family tem
#'
#' @examples
#' demo_stim() |> centroid()
#' 
#' # get the centre of the eye points
#' demo_stim() |> centroid(0:1)
centroid <- function(stimuli, points = NULL) {
  stimuli <- require_tems(stimuli)
  
  pts <- lapply(stimuli, `[[`, "points")
  if (!is.null(points)) {
    pts <- lapply(pts, `[`, , points+1, drop = FALSE)
  }
  
  sapply(pts, apply, 1, mean) |> t()
}

