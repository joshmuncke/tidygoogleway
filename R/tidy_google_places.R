#' Tidy Google Places
#'
#' This function calls the Google Places API using the
#' googleway::google_places function. It will return a single,
#' tidy dataframe instead of a json object. Result selection
#' is done using string distance similarity.
#'
#' @param search_name The name of the place to search
#' @param search_address The address (partial or full) of the place to search
#' @param search_lat The latitude of the place to search (note - only used for
#' distance matching, not radial search - use location)
#' @param search_lng The latitude of the place to search (note - only used for
#' distance matching, not radial search - use location)
#' @param keep_top Should only the best matching result be returned?
#' @param key \code{string} A valid Google Developers Places API key.
#' @param ... Other arguments passed to googleway
#'
#' @export
tidy_google_places <- function(search_name = NULL,
                               search_address = NULL,
                               search_lat = NULL,
                               search_lng = NULL,
                               keep_top = TRUE,
                               key = NULL,
                               ...) {

  # Combine name and address into single string
  search_string <- paste0(search_name, ", ", search_address)

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
                              search_lat = nulltona(search_lat),
                              search_lng = nulltona(search_lng),
                              place_id = nulltona(unlisted_result$results.place_id[[i]]),
                              place_name = nulltona(unlisted_result$results.name[[i]]),
                              address = nulltona(unlisted_result$results.formatted_address[[i]]),
                              latitude = nulltona(unlisted_result$results.geometry$location$lat[[i]]),
                              longitude = nulltona(unlisted_result$results.geometry$location$lng[[i]]),
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
             geo_distance_metres = ifelse(is.na(great_circle(search_lat, search_lng, latitude, longitude)),0.0000001,great_circle(search_lat, search_lng, latitude, longitude)),
             # geo_similarity_scaled = 1 - (geo_distance_metres - min(geo_distance_metres)) / (max(geo_distance_metres) - min(geo_distance_metres)),
             # geometric mean of distances
             mean_similarity = (name_distance * address_distance * geo_distance_metres)^(1/3))
  }

  if(keep_top) {
    google_results_flattened <- google_results_flattened %>% dplyr::arrange(mean_similarity) %>% dplyr::slice(1)
  }

  # These helper functions exist in googleway
  # Future version use them?
  # place_ids <- googleway::access_result(unformatted_result, "place") %>% tibble::enframe(name = NULL, value = "place_id")

  google_results_flattened
}

#' Append tidy google places to a dataframe
#'
#' Appends the best matching google places result to
#' a dataframe of lookup locations.
#'
#'@export
add_tidy_google_places <- function(df,
                                 name,
                                 address,
                                 lat,
                                 lng,
                                 key) {
#furrr::future_pmap_dfr()
  name_field <- rlang::enquo(name)
  address_field <- rlang::enquo(address)
  lat_field <- rlang::enquo(lat)
  lng_field <- rlang::enquo(lng)

  search_df <- df %>% dplyr::select(search_name = !! name_field,
<<<<<<< HEAD
                      search_address = !! address_field) %>%
    dplyr::mutate(search_lat = NULL,
                  search_lng = NULL,
                  keep_top = TRUE,
                  key = mykey)

  # search_df
  google_results <- search_df %>% furrr::future_pmap_dfr(tidy_google_places)
  #
  df %>% dplyr::bind_cols(google_results)
=======
                       search_address = !! address_field
                       # search_lat = !! lat_field,
                       # search_lng = !! lng_field,
  ) %>% dplyr::mutate(key = mykey)

  # search_df
  google_results <- search_df %>% furrr::future_map_dfr(tidy_google_places)
  #
  google_results
>>>>>>> 77d42c77edd565571a8d968d69ff1a6f725f3d21
}


