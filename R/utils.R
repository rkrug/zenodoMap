#' Provide a default for NULL or empty values
#'
#' @param a Candidate value.
#' @param b Default value.
#' @return `a` if present, otherwise `b`.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
