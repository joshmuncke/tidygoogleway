nulltona <- function(x) {
  ifelse(purrr::is_null(x), NA, x)
}
