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
      lapply(as.integer)
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
#' s <- demo_stim()[1] %>%
#'   subset_tem(features("face"))
#' s[[1]]$lines
#' 
#' # remove all lines
#' s2 <- s %>%
#'   change_lines(line_id = 1:8, pts = NULL)
#' s2[[1]]$lines
#' 
#' draw_tem(s2, pt.shape = "index", pt.size = 10)
#' 
#' # add new line
#' s3 <- s2 %>%
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
features <- function(..., tem_id = c("FRL", "dlib70")) {
  # 0-based for compatibility with webmorph
  tem_id <- match.arg(tem_id)
  
  named_features <- list(...) %>% unlist()
  
  if (tem_id == "FRL") {
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
  features[named_features] %>%
    unlist() %>% unname() %>%
    unique() %>% sort()
}


#' Templates to XML
#' 
#' Make an XML file with the template points for a set of stimuli. For use with training dlib.
#'
#' @param stimuli list of class stimlist
#' @param dir path to save XML file and images
#'
#' @export
#'
tem_to_xml <- function(stimuli, dir = "images") {
  stimuli <- validate_stimlist(stimuli)
  
  # write images to dir ----
  if (wm_opts("verbose")) message("Writing images to directory")
  
  paths <- stimuli %>%
    remove_tem() %>%
    write_stim(dir, format = "jpg") %>%
    unlist() %>% # gets rid of NULL entries for tems
    sapply(normalizePath)
  
  # get bounding boxes ----
  if (wm_opts("verbose")) {
    pb <- progress::progress_bar$new(
      total = length(paths), clear = FALSE,
      format = "Detecting face locations [:bar] :current/:total :elapsedfull"
    )
    pb$tick(0)
    Sys.sleep(0.5)
    pb$tick(0)
  }
  
  pyscript <- system.file("python/facedetect.py", package = "webmorphR")
  reticulate::source_python(pyscript)
  boxes <- lapply(paths, function(p) {
      if (wm_opts("verbose")) pb$tick()
      get_location(p)
    }) %>%
    lapply(unlist)
  
  # create XML ----
  if (wm_opts("verbose")) {
    pb <- progress::progress_bar$new(
      total = length(paths), clear = FALSE,
      format = "Creating XML file [:bar] :current/:total :elapsedfull"
    )
    pb$tick(0)
    Sys.sleep(0.5)
    pb$tick(0)
  }
  
  imgs <- mapply(function(stim, path, box) {
    if (wm_opts("verbose")) pb$tick()
    
    i <- -1
    pts <- apply(stim$points, 2, function(pt) { 
      i <<- i + 1
      sprintf("<part name='%03.f' x='%.0f' y='%.0f'/>", i, 
              round(pt["x"]), round(pt["y"]))
    }) %>%
      paste(collapse = "\n      ")
    
    if (is.null(box)) {
      minpt <- apply(stim$points, 1, min)
      maxpt <- apply(stim$points, 1, max)
      box <- c(top = max(0, minpt["y"]-10), 
               right = min(stim$width, maxpt["x"]+10), 
               bottom = min(stim$height, maxpt["y"]+10), 
               left = max(0, minpt["x"]-10)
               ) %>% round()
    }
    
    sprintf("  <image file='%s'>
    <box top='%d' left='%d' width='%d' height='%d'>
      %s
    </box>
  </image>", path, 
            box[[1]], box[[4]], 
            abs(box[[2]]-box[[4]]), 
            abs(box[[3]]-box[[1]]), pts)
  }, stimuli, paths, boxes) %>%
    paste(collapse = "\n")
  
  # write XML ----
  xml <- sprintf("<?xml version='1.0' encoding='ISO-8859-1'?>
<?xml-stylesheet type='text/xsl' href='image_metadata_stylesheet.xsl'?>
<dataset>
<name>%s</name>
<images>
%s
</images>
</dataset>", "Image Set", imgs)
  
  write(xml, file.path(dir, "images.xml"))
}

