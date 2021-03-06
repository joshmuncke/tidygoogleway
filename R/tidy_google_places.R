#' Get tidy Google Places information for a single location
#'
#' This function calls the Google Places API using the
#' googleway::google_places function. It will return a single,
#' tidy dataframe instead of a json object. Result selection
#' is done using string and geographic distances.
#'
#' @param search_name The name of the place to search
#' @param search_address The address (partial or full) of the place to search
#' @param search_latitude The latitude of the place to search (note - only used for
#' distance matching, not radial search)
#' @param search_longitude The latitude of the place to search (note - only used for
#' distance matching, not radial search)
#' @param key A valid Google Developers Places API key.
#' @param .keep_all Toggle whether multiple results will be returned (if found)
#' @param ... Other arguments passed to googleway::google_places
#'
#' @export
get_tidy_google_place <- function(search_name = NULL,
                               search_address = NULL,
                               search_latitude = NULL,
                               search_longitude = NULL,
                               key = googleway:::get_api_key("places"),
                               .keep_all = FALSE,
                               ...) {

  # Location name and address must be provided
  if(is.null(search_name) && is.null(search_address)) {stop("You must provide a search name or address term", call. = F)}

  # If either latitude or longitude is provided the other must be too
  if(xor(!is.numeric(search_latitude), !is.numeric(search_longitude))) {stop("Cannot provide single geolocation co-ordinate", call. = F)}

  # Combine name and address into single string
  search_string <- paste0(nulltoblankstring(search_name), ", ", nulltoblankstring(search_address))

  # Call googleway::google_places
  unformatted_result <- googleway::google_places(search_string, key = key, ...)

  # Create a blank tibble for structured results
  google_results_flattened <- tibble::tibble()

  if(unformatted_result$status == 'OK') {

    # Unlist this one level
    unlisted_result <- unformatted_result %>% unlist(recursive = F)
    num_results <- length(unlisted_result$results.place_id)

    for(i in 1:num_results) {
      # Create a new dummy row
      new_row <- tibble::tibble(search_name = nulltona(search_name),
                                search_address = nulltona(search_address),
                                search_latitude = nulltona(search_latitude),
                                search_longitude = nulltona(search_longitude),
                                google_place_id = nulltona(unlisted_result$results.place_id[[i]]),
                                google_place_name = nulltona(unlisted_result$results.name[[i]]),
                                google_address = nulltona(unlisted_result$results.formatted_address[[i]]),
                                google_latitude = nulltona(unlisted_result$results.geometry$location$lat[[i]]),
                                google_longitude = nulltona(unlisted_result$results.geometry$location$lng[[i]]),
                                google_viewport_ne_lat = nulltona(unlisted_result$results.geometry$viewport$northeast$lat[[i]]),
                                google_viewport_ne_lng = nulltona(unlisted_result$results.geometry$viewport$northeast$lng[[i]]),
                                google_viewport_sw_lat = nulltona(unlisted_result$results.geometry$viewport$southwest$lat[[i]]),
                                google_viewport_sw_lng = nulltona(unlisted_result$results.geometry$viewport$southwest$lng[[i]]),
                                google_price_level = nulltona(unlisted_result$results.price_level[[i]]),
                                google_rating = nulltona(unlisted_result$results.rating[[i]]),
                                google_user_ratings_total = nulltona(unlisted_result$results.user_ratings_total[[i]]),
                                google_types = nulltona(unlisted_result$results.types[[i]]),
                                google_permanently_closed = nulltona(unlisted_result$results.permanently_closed[[i]]),
                                google_result_number = i,
                                google_n_results = num_results)

      # Bind this new row into our structured dataset
      google_results_flattened <- google_results_flattened %>% dplyr::bind_rows(new_row)
    }

    # Add similarity calculations
    google_results_flattened <- google_results_flattened %>%
      dplyr::rowwise() %>%
      dplyr::mutate(name_distance = ifelse(is.null(search_name), NA_real_, stringdist::stringdist(stringr::str_to_lower(search_name), stringr::str_to_lower(google_place_name), method = "jw"))) %>%
      dplyr::mutate(address_distance = ifelse(is.null(search_address), NA_real_, stringdist::stringdist(stringr::str_to_lower(search_address), stringr::str_to_lower(google_address), method = "jw"))) %>%
      dplyr::mutate(geo_distance_metres = ifelse(!is.numeric(search_latitude) | !is.numeric(search_longitude) | !is.numeric(google_latitude) | !is.numeric(google_longitude), NA_real_, great_circle(search_latitude, search_longitude, google_latitude, google_longitude)))

    # Now calculate the geometric mean of these three distance metrics
    # Use geo-mean so we can average distance measures on different scales
    # Helper function will ignore any NAs
    google_results_flattened <- google_results_flattened %>%
      dplyr::mutate(mean_distance = gm_mean(c(name_distance, address_distance, geo_distance_metres)))%>%
      dplyr::ungroup()

    # Filter just the top result by our similarity metric (if asked for)
    # We check the variance of the similarities because if that is 0 (i.e. all sims are equal)
    #  then we don't want to do any re-ordering - pick the first one suggested by Google
    if(!.keep_all & nrow(google_results_flattened) > 1) {
      if(var(google_results_flattened$mean_distance) == 0) {
        google_results_flattened <- google_results_flattened %>% dplyr::slice(1)
      }
      else {
        google_results_flattened <- google_results_flattened %>% dplyr::arrange(mean_distance) %>% dplyr::slice(1)
      }
    }
  }

  else {
    blank_row =  tibble::tibble(search_name = nulltona(search_name),
                                search_address = nulltona(search_address),
                                search_latitude = nulltona(search_latitude),
                                search_longitude = nulltona(search_longitude),
                                google_place_id = NA,
                                google_place_name = NA,
                                google_address = NA,
                                google_latitude = NA,
                                google_longitude = NA,
                                google_viewport_ne_lat = NA,
                                google_viewport_ne_lng = NA,
                                google_viewport_sw_lat = NA,
                                google_viewport_sw_lng = NA,
                                google_price_level = NA,
                                google_rating = NA,
                                google_user_ratings_total = NA,
                                google_types = NA,
                                google_permanently_closed = NA,
                                google_result_number = NA,
                                google_n_results = 0,
                                name_distance = NA,
                                address_distance = NA,
                                geo_distance_metres = NA,
                                mean_distance = NA
                                )

    google_results_flattened <- google_results_flattened %>% dplyr::bind_rows(blank_row)
  }

  google_results_flattened
}


#' Append Google Places data to a dataframe
#'
#' This function takes a dataframe containing location information and
#' returns the same dataframe with additional enriched Google Places data.
#' If multiple results are found they will be compared/selected using
#' string similarity and geographic distance.
#'
#' @param search_name Field containing the name of the location
#' @param search_address Field containing the address of the location
#' @param search_latitude Field containing the latitude of the location (for distance comparison)
#' @param search_longitude Field containing the longitude of the location (for distance comparison)
#' @param key A valid Google Developers Places API key.
#' @param .keep_all Toggle whether multiple results will be returned (if found)
#' @param ... Other arguments passed to googleway::google_places
#'
#' @export
add_google_places <- function(df, search_name, search_address, search_latitude, search_longitude, key = googleway:::get_api_key("places"), .keep_all = FALSE, ...) {
  # Check that at least a name or address has been provided
  if(missing(search_name) && missing(search_address)) {stop("You must provide a search name or address term", call. = F)}

  # If either latitude or longitude is provided the other must be too
  if(xor(missing(search_latitude), missing(search_longitude))) {stop("Cannot provide single geolocation co-ordinate", call. = F)}

  # Get the input dataframe as supplied by user
  if(missing(search_name)) { name = NULL }
  else { name = enquo(search_name) }

  # Get the input dataframe as supplied by user
  if(missing(search_address)) { add = NULL }
  else { add = enquo(search_address) }

  # Get the input dataframe as supplied by user
  if(missing(search_latitude)) { lat = NULL }
  else { lat = enquo(search_latitude) }

  # Get the input dataframe as supplied by user
  if(missing(search_longitude)) { lng = NULL }
  else { lng = enquo(search_longitude) }

  # Rename input dataframe columns
  working_df <- df %>%
    dplyr::select(search_name = !!name,
                  search_address = !!add,
                  search_latitude = !!lat,
                  search_longitude = !!lng) %>%
    dplyr::mutate(key = key,
                  .keep_all = .keep_all)

  # Map the get_tidy_google_place function to our working dataframe
  working_df <- working_df %>%
    furrr::future_pmap_dfr(get_tidy_google_place, .id = "row_number", .progress = TRUE) %>%
    mutate(row_number = as.integer(row_number))

  # Check for clashing variable names
  matching_names <- intersect(names(df), names(working_df))
  if(length(matching_names) > 0) warning("Overlapping column names between input data and Google results - columns will be renamed", call. = F)

  results_df <- df %>%
    mutate(row_number = row_number()) %>%
    left_join(working_df, by = "row_number") %>%
    select(-row_number, -search_name, -search_address, -search_latitude, -search_longitude)

  results_df
}
