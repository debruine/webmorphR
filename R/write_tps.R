#' Create a TPS file from a stimlist
#'
#' @param stimuli list of stimuli
#' @param path_to_tps optional filename to save TPS file
#'
#' @return text of tps file
#' @export
#'
#' @examples
#' # set path_to_tps to save to a file
#' demo_stim() |>
#'   write_tps() |>
#'   cat()
#'
write_tps <- function(stimuli, path_to_tps = NULL) {
  stimuli <- as_stimlist(stimuli)

  tps <- mapply(function(stim, name) {
    pt <- {stim$points * c(1, -1)} |>
      t() |> as.data.frame()

    pt_list <- paste(pt[[1]], pt[[2]], sep = "\t") |>
      paste(collapse = "\n")

    sprintf("LM=%i\n%s\nID=%s",
            ncol(stim$points),
            pt_list,
            name)
  }, stimuli, names(stimuli) %||% seq_along(stimuli)) |>
    paste(collapse = "\n")

  if (is.null(path_to_tps)) {
    return(tps)
  } else {
    write(tps, path_to_tps)
    stimuli
  }
}

#' Convert stimuli to array for geomorph
#'
#' @param stimuli list of stimuli
#'
#' @return 3D array
#' @export
#'
#' @examples
#' data <- demo_stim() |> tems_to_array()
#' dim(data)
#'
tems_to_array <- function(stimuli) {
  stimuli <- require_tems(stimuli, TRUE)

  # check number of points
  n_pts <- lapply(stimuli, `[[`, "points") |>
    sapply(ncol) |>
    unique()

  if (is.null(n_pts[[1]])) {
    stop("No image had templates")
  } else if (length(n_pts) > 1) {
    stop("Each tem must have the same length")
  }

  sapply(stimuli, function(tem) {
    t(tem$points * c(1, -1))
  }) |>
    array(dim = c(n_pts, 2, length(stimuli)),
          dimnames = list(NULL, c("X", "Y"), names(stimuli)))
}
