#' Piped OR
#'
#' LHS if not \code{NULL}, otherwise RHS
#'
#' @param l LHS.
#' @param r RHS.
#' @return LHS if not \code{NULL}, otherwise RHS.
#' @name OR
#'
#' @keywords internal
#'
`%||%` <- function(l, r) {
  if (is.null(l)) r else l
}

.b <- `[`
.bb <- `[[`


