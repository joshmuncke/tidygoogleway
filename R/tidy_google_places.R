#' Add Google Places data to a dataframe
#'
#' This function calls the Google Places API using the
#' googleway::google_places function. It will return a single,
#' tidy dataframe instead of a json object. Result selection
#' is done using string distance similarity.
#'
#' @param search_name The name of the place to search
#' @param search_address The address (partial or full) of the place to search
#' @param search_latitude The latitude of the place to search (note - only used for
#' distance matching, not radial search)
#' @param search_longitude The latitude of the place to search (note - only used for
#' distance matching, not radial search)
#' @param key \code{string} A valid Google Developers Places API key.
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
  if(is.null(search_name) && is.null(search_address)) {stop("You must provide a search term to Google Places", call. = F)}

  # If either latitude or longitude is provided the other must be too
  if(xor(!is.numeric(search_latitude), !is.numeric(search_longitude))) {stop("Cannot provide single geolocation co-ordinate", call. = F)}

  # Combine name and address into single string
  search_string <- paste0(nulltoblankstring(search_name), ", ", nulltoblankstring(search_address))

  # Call googleway::google_places
  unformatted_result <- googleway::google_places(search_string, key = key, ...)

  # Unlist this one level
  unlisted_result <- unformatted_result %>% unlist(recursive = F)
  num_results <- length(unlisted_result$results.id)

  # Create a blank tibble for structured results
  google_results_flattened <- tibble::tibble()

  for(i in 1:num_results) {
    # Create a new dummy row
    new_row <- tibble::tibble(search_name = search_name,
                              search_address = search_address,
                              search_latitude = nulltona(search_latitude),
                              search_longitude = nulltona(search_longitude),
                              place_id = nulltona(unlisted_result$results.place_id[[i]]),
                              place_name = nulltona(unlisted_result$results.name[[i]]),
                              address = nulltona(unlisted_result$results.formatted_address[[i]]),
                              latitude = nulltona(unlisted_result$results.geometry$location$lat[[i]]),
                              longitude = nulltona(unlisted_result$results.geometry$location$lng[[i]]),
                              viewport_ne_lat = nulltona(unlisted_result$results.geometry$viewport$northeast$lat[[i]]),
                              viewport_ne_lng = nulltona(unlisted_result$results.geometry$viewport$northeast$lng[[i]]),
                              viewport_sw_lat = nulltona(unlisted_result$results.geometry$viewport$southwest$lat[[i]]),
                              viewport_sw_lng = nulltona(unlisted_result$results.geometry$viewport$southwest$lng[[i]]),
                              price_level = nulltona(unlisted_result$results.price_level[[i]]),
                              rating = nulltona(unlisted_result$results.rating[[i]]),
                              user_ratings_total = nulltona(unlisted_result$results.user_ratings_total[[i]]),
                              types = nulltona(unlisted_result$results.types[[i]]),
                              permanently_closed = nulltona(unlisted_result$results.permanently_closed[[i]]),
                              result_number = i,
                              n_results = num_results)

    # Bind this new row into our structured dataset
    google_results_flattened <- google_results_flattened %>% dplyr::bind_rows(new_row)

    # Add similarity calculations
    google_results_flattened <- google_results_flattened %>%
      dplyr::mutate(name_distance = 0.0000001 + stringdist::stringdist(stringr::str_to_lower(search_name), stringr::str_to_lower(place_name), method = "jw"),
             address_distance = 0.0000001 + stringdist::stringdist(stringr::str_to_lower(search_address), stringr::str_to_lower(address), method = "jw"),
             geo_distance_metres = ifelse(is.na(great_circle(search_latitude, search_longitude, latitude, longitude)),0.0000001,great_circle(search_latitude, search_longitude, latitude, longitude)),
             # geo_similarity_scaled = 1 - (geo_distance_metres - min(geo_distance_metres)) / (max(geo_distance_metres) - min(geo_distance_metres)),
             # geometric mean of distances
             mean_similarity = (name_distance * address_distance * geo_distance_metres)^(1/3))
  }

  if(!.keep_all) {
    google_results_flattened <- google_results_flattened %>% dplyr::arrange(mean_similarity) %>% dplyr::slice(1)
  }

  # These helper functions exist in googleway
  # Future version use them?
  # place_ids <- googleway::access_result(unformatted_result, "place") %>% tibble::enframe(name = NULL, value = "place_id")

  google_results_flattened
}


#' @export
add_google_places <- function(df, search_name, search_address, search_latitude, search_longitude, key = googleway::get_api_key("places"), .keep_all = FALSE, ...) {
  df
}
