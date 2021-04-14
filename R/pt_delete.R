#' Delete template points
#'
#' @param stimuli list of class stimlist
#' @param ... vectors of points to delete
#'
#' @return stimlist with altered templates
#' @export
#'
#' @examples
#' demo_stim()[1] %>%
#'   pt_delete(frl_features("imprecise")) %>%
#'   draw_tem()
#'
pt_delete <- function(stimuli, ...) {
  stimuli <- validate_stimlist(stimuli)
  points_to_del <- list(...) %>% unlist() %>% unique() %>% sort()

  for (i in seq_along(stimuli)) {
    oldpts <- stimuli[[i]]$points
    oldlines <- stimuli[[i]]$lines
    oldclosed <- stimuli[[i]]$closed

    full_idx <- 1:dim(oldpts)[[2]]

    # remove points, webmorph points are 0-indexed so +1
    keep_idx <- setdiff(full_idx, points_to_del+1)
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

#' Get point indices for FRL features
#'
#' @param ... a vector of feature names or "imprecise" to choose undereyes, ears, halo, mouth2, smile_lines, cheekbones, philtrum, chin, neck points
#'
#' @return vector of corresponding FRL template indices
#' @export
#'
#' @examples
#' frl_features("mouth")
#' frl_features("left_eye", "right_eye")
#'
frl_features <- function(...) {
  # 0-based for compatibility with webmorph

  named_features <- list(...) %>% unlist()

  features <- list(
    # imprecise (-1 because Iris)
    undereyes = c(45:50) - 1,
    ears = c(116:125) - 1,
    halo = c(146:158) - 1,
    mouth2 = c(100:104) - 1,
    smile_lines = c(159:164) - 1,
    cheekbones = c(165:170) - 1,
    philtrum = c(171:174) - 1,
    chin = c(175:179) - 1,
    neck = c(184:185, 145, 157) - 1,
    # features (no -1 because Lisa)
    eyes = c(0, 2:9, 18:22, 28:30, 34:38, 1, 10:17, 23:27, 31:33, 39:43),
    left_eye = c(0, 2:9, 18:22, 28:30, 34:38),
    right_eye = c(1, 10:17, 23:27, 31:33, 39:43),
    brows = c(71:76, 83:84, 77:82, 85:86),
    left_brow = c(71:76, 83:84),
    right_brow = c(77:82, 85:86),
    nose = c(50:70, 170, 172, 179:182),
    mouth = c(87:108),
    face = c(109:114, 125:144, 185:188)
  )

  if ("imprecise" %in% named_features) {
    named_features <- c(
      named_features, "undereyes", "ears", "halo",
      "mouth2", "smile_lines", "cheekbones",
      "philtrum", "chin", "neck"
    )
  }

  # unavailable features are ignored

  features[named_features] %>%
    unlist() %>% unname() %>%
    unique() %>% sort()
}

