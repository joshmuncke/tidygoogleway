# For handling NULLs in places search results
nulltona <- function(x) {
  ifelse(purrr::is_null(x), NA, x)
}

# For handling NULLs in search terms
nulltoblankstring <- function(x) {
  ifelse(purrr::is_null(x), "", x)
}

# For calculating similarity across string/geo distances
gm_mean = function(x) {
  # Remove NAs
  x_no_na <- x[!is.na(x)]

  # Add a v small amount (to prevent zeros/perfect matches from killing the average)
  x_shifted <- x_no_na + 1E-9

  exp(sum(log(x_shifted), na.rm = TRUE) / length(x_shifted))
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
