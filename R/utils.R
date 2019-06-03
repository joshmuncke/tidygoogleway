nulltona <- function(x) {
  ifelse(purrr::is_null(x), NA, x)
}

# Converts degrees to radians
deg2rad <- function(deg) {(deg * pi) / (180)}

# Calculates the great circle distance between two points in metres
great_circle <- function(lat1, lng1, lat2, lng2) {
  6371000 * acos(cos(deg2rad(90 - lat1)) * cos(deg2rad(90 - lat2)) + sin(deg2rad(90 - lat1)) * sin(deg2rad(90 - lat2)) * cos(deg2rad(lng1 - lng2)))
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
