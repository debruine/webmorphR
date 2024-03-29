#' Set stimulus names in a stimlist
#'
#' @param stimuli A list of stimuli
#' @param new_names Vector of new names - must be the same length as the stimlist
#' @param prefix String to prefix to each name
#' @param suffix String to append to each name
#' @param pattern Pattern for gsub
#' @param replacement Replacement for gsub
#' @param ... Additional arguments to pass on to `base::gsub()`
#'
#' @return A list of stimuli with the new names
#' @export
#' @family info
#'
#' @examples
#' demo_stim() |>
#'   rename_stim(prefix = "new_") |>
#'   names()
rename_stim <- function(stimuli, new_names = NULL, prefix = "", suffix = "",
                   pattern = NULL, replacement = NULL, ...) {
  stimuli <- as_stimlist(stimuli)

  if (is.null(new_names)) {
    new_names <- names(stimuli)
  } else if (length(new_names) != length(stimuli)) {
    stop("The length of new_names must be equal to the length of stimuli")
  }

  # search and replace strings
  if (!is.null(pattern) && !is.null(replacement)) {
    new_names <- gsub(pattern, replacement,
                      new_names, ...)
  }

  # add prefix and/or suffix
  new_names <- paste0(prefix, new_names, suffix)

  names(stimuli) <- new_names
  for (i in seq_along(stimuli)) {
    stimuli[[i]]$name <- new_names[i]
  }

  stimuli
}
