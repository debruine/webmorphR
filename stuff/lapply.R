#' Apply a Function over a List or Vector
#'
#' This function behaves exactly like \code{\link[base]{lapply}}, but returns a list with class stimlist if the objects returned by lapply are webmorph stimuli (with class stim).
#'
#' @param X a list
#' @param FUN the function to be applied to each element of X
#' @param ... optional arguments to FUN.
#'
#' @return a list that is class stimlist is the returned elements are class stim
#' @export

lapply <- function (X, FUN, ...) {
  Y <- base::lapply(X, FUN, ...)
  is_stimlist <- FALSE

  for (i in seq_along(Y)) {
    is_stimlist <- is_stimlist || "stim" %in% class(Y[[i]])
  }
  if (is_stimlist) class(Y) <- c("stimlist", "list")

  Y
}
