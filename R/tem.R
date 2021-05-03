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
#' frl <- tem_def("FRL")
#' frl$points[1:10, ]
#'
#' fpp106 <- tem_def("fpp106")
#' fpp106$lines %>% str()
#'
#' fpp83 <- tem_def("fpp83")
#' fpp83$mask %>% str()
#'
tem_def <- function(tem_id = "FRL", path = NULL) {

  # read file or url ----
  if (!is.null(path)) {
    if (!file.exists(path)) {
      stop(sprintf("The file at %s does not exist", path))
    }
    tem_def <- tryCatch({
      readLines(path)
    }, error = function(e) {
      stop("The file couldn't be read")
    })
  } else if (is.numeric(tem_id)) {
    url <- sprintf("https://webmorph.org/scripts/temDownload?tem_id=%d",
                   tem_id)
    tem_def <- tryCatch({
      readLines(url)
    }, error = function(e) {
      stop("You might not have an internet connection")
    })
  } else if (is.character(tem_id)) {
    temdir <- system.file("extdata/tem_defs", package = "webmorphR")
    temdefs <- list.files(temdir, full.names = TRUE)
    match <- grepl(tolower(tem_id), tolower(temdefs), fixed = TRUE)
    if (all(match == FALSE)) {
      stop(tem_id, " is not a built-in template")
    }

    tem_def <- readLines(temdefs[match][[1]])
  } else {
    stop("You must supply a numeric tem_id or a valid path to a template definition file.")
  }

  # parse text ----
  infotable <- utils::read.csv(text = tem_def[1:8],
                        header = FALSE,
                        row.names = 1) %>%
    t() %>% as.data.frame()
  tem <- list()
  for (i in 1:8) {
    tem[i] <- strsplit(infotable[ , i], ";") %>%
      utils::type.convert(as.is = TRUE)
  }
  names(tem) <- colnames(infotable)

  # get points (required)
  ptstart <- grepl("n\\s*,\\s*name\\s*,\\s*x\\s*,\\s*y\\s*,\\s*sym\\s*", tem_def) %>% which()
  if (length(ptstart) == 0) {
    stop("No points were found in the template definition")
  }
  ptrange <- ptstart:(ptstart+tem$points)
  tem$points <- utils::read.csv(text = tem_def[ptrange])

  # get lines (optional)
  lnstart <- grepl("n\\s*,\\s*linetype\\s*,\\s*color\\s*,\\s*points\\s*", tem_def) %>% which()
  if (length(lnstart) == 1) {
    linerange <- lnstart:(lnstart+tem$lines)
    lines <- utils::read.csv(text = tem_def[linerange]) %>%
      dplyr::arrange(.data$n) # probably already in order, but make sure
    tem$lines <- sapply(lines$points, strsplit, ",", USE.NAMES = FALSE) %>%
      sapply(as.integer)
    tem$closed <- lines$linetype == "closed"
    tem$linecolor <- lines$color
  } else if (tem$lines == 0) {
    tem$lines <- NULL
  } else {
    warning("No line definitions were found, but the template is meant to have ", tem$lines, " lines")
    tem$lines <- NULL
  }

  # get masks (optional)
  maskstart <- grepl("mask\\s*,\\s*points\\s*", tem_def) %>% which()
  if (length(maskstart) == 1) {
    maskrange <- maskstart:length(tem_def)
    m <- utils::read.csv(text = tem_def[maskrange])
    tem$masks <- lapply(m$points, strsplit, "\\s*;\\s*") %>%
      lapply(sapply, strsplit, "\\s*,\\s*") %>%
      lapply(sapply, as.integer, simplify = FALSE)
    names(tem$masks) <- m$mask
  }

  tem
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
#' demo_stim()[1] %>%
#'   subset_tem(features("gmm")) %>%
#'   draw_tem()
#'
subset_tem <- function(stimuli, ..., keep = TRUE) {
  stimuli <- validate_stimlist(stimuli)
  points <- list(...) %>% unlist() %>% unique() %>% sort()
  
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
    if (setdiff(keep_idx, full_idx) %>% length() > 0) {
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
      l <- trans[(x + 1)] %>%
        stats::na.omit() %>%
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
#' Available features for the FRL template are: "gmm", "oval", "face", "mouth", "nose", "eyes", "brows", "left_eye",  "right_eye", "left_brow",  "right_brow", "ears", "undereyes", "teeth", "smile_lines", "cheekbones", "philtrum", "chin", "neck", "halo".
#'
#' @param ... a vector of feature names (see Details)
#' @param tem_id template ID (currently only works for FRL)
#'
#' @return vector of corresponding FRL template indices
#' @export
#'
#' @examples
#' features("mouth")
#' features("gmm")
#'
features <- function(..., tem_id = "FRL") {
  # 0-based for compatibility with webmorph
  
  named_features <- list(...) %>% unlist()
  
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
  
  # combo features
  features$eyes <- c(features$left_eye, features$right_eye)
  features$brows <- c(features$left_brow, features$right_brow)
  features$oval <- c(features$halo, features$neck)
  features$gmm <- c(features$face, features$eyes, features$brows, 
                    features$nose, features$mouth)
  
  # unavailable and duplicate features are ignored
  features[named_features] %>%
    unlist() %>% unname() %>%
    unique() %>% sort()
}

