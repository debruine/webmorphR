#' Get template definition
#'
#' Template definitions are specially formatted files that contain information about templates that are needed to do things like symmetrising and masking images. This function is mostly used internally.
#'
#' @param tem_id numeric ID of template to retrieve from webmorph.org
#' @param path path of local tem definition file
#'
#' @return list with template definition
#' @export
#'
#' @examples
#' frl <- tem_def("frl")
#' frl$points[1:10, ]
#'
#' fpp106 <- tem_def("fpp106")
#' fpp106$lines |> str()
#'
#' fpp83 <- tem_def("fpp83")
#' fpp83$mask |> str()
#'
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


#' Change the points in a line
#'
#' @param stimuli list of class stimlist
#' @param line_id IDs of the lines to change
#' @param pts vector of points to change the line_id to (deletes line if NULL)
#'
#' @return stimlist with altered templates
#' @export
#'
#' @examples
#' s <- demo_stim()[1] |>
#'   subset_tem(features("face"))
#' s[[1]]$lines
#' 
#' # remove all lines
#' s2 <- s |>
#'   change_lines(line_id = 1:8, pts = NULL)
#' s2[[1]]$lines
#' 
#' draw_tem(s2, pt.shape = "index", pt.size = 10)
#' 
#' # add new line
#' s3 <- s2 |>
#'   change_lines(line_id = 1, pts = c(0, 26, 1, 27, 2, 6:14, 5, 29, 4, 28, 3, 25:15, 0))
#' s3[[1]]$lines
#' draw_tem(s3, line.color = "hotpink", line.alpha = 1)
change_lines <- function(stimuli, line_id = 1, pts = NULL) {
  for (i in seq_along(stimuli)) {
    oldlines <- stimuli[[i]]$lines
    if (is.null(pts)) {
      oldlines[line_id] <- pts
    } else {
      oldlines[[line_id]] <- pts
    }
    stimuli[[i]]$lines <- oldlines
  }
  
  stimuli
}

#' Subset template points
#'
#' @param stimuli list of class stimlist
#' @param ... vectors of points to keep or delete
#' @param keep whether to keep or delete the points
#'
#' @return stimlist with altered templates
#' @export
#'
#' @examples
#' demo_stim()[1] |>
#'   subset_tem(features("gmm")) |>
#'   draw_tem()
#'
subset_tem <- function(stimuli, ..., keep = TRUE) {
  stimuli <- validate_stimlist(stimuli, tem = TRUE)
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
#' @param stimuli list of class stimlist
#'
#' @return list of class stimlist
#' @export
#'
#' @examples
#' demo_stim() |> remove_tem()
remove_tem <- function(stimuli) {
  stimuli <- validate_stimlist(stimuli)
  
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
#' @param stimuli list of class stimlist
#'
#' @return logical
#' @export
#'
#' @examples
#' stim <- demo_stim()
#' stim2 <- subset_tem(stim, features("gmm"))
#' 
#' same_tems(stim)
#' 
#' c(stim, stim2) |> same_tems()
same_tems <- function(stimuli) {
  stimuli <- validate_stimlist(stimuli)
  
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
#' @param stimuli list of class stimlist with templates
#' @param pt point(s) to return
#'
#' @return data frame of x and y coordinates of the specified point(s) for each stimulus
#' @export
#'
#' @examples
#' demo_stim() |> get_point(0:1)
get_point <- function(stimuli, pt = 0) {
  stimuli <- validate_stimlist(stimuli, TRUE)
  
  all_pts <- tems_to_array(stimuli)
  # this function reverses y-coordinates
  
  data.frame(
    image = rep(names(stimuli), each = length(pt)),
    point = rep(pt, times = length(stimuli)),
    x = all_pts[pt+1, "X", ] |> matrix(ncol = 1),
    y = all_pts[pt+1, "Y", ] |> matrix(ncol = 1) * -1
  )
}
