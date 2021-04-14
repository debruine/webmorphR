#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Piped OR
#'
#' LHS if not \code{NULL}, otherwise RHS
#'
#' @param l LHS.
#' @param r RHS.
#' @return LHS if not \code{NULL}, otherwise RHS.
#' @name OR
#'
#' @export
#'
#' @examples
#' x <- list(b = 2, c = 3)
#' x$a %||% x$b %||% x$c
#' x$a %||% "default_value"
#'
`%||%` <- function(l, r) {
  if (is.null(l)) r else l
}


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
