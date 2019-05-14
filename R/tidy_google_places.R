#' Tidy Google Places
#'
#' This function calls the Google Places API using the
#' googleway::google_places function. It will always
#' return a single, tidy dataframe instead of a json
#' object. Result selection is done using string
#' distance similarity.
#'
#' #' @param search_string \code{string} A search term representing a place for
#' which to search. If blank, the \code{location} argument must be used.
#' @param location \code{numeric} vector of latitude/longitude coordinates
#' (in that order) around which to retrieve place information.
#' @param radius \code{numeric} Defines the distance (in meters) within which to
#' return place results. Required if only a \code{location} search is specified.
#' The maximum allowed radius is 50,000 meters. Radius must not be included if
#' \code{rankby} is used. see Details.
#' @param rankby \code{string} Specifies the order in which results are listed.
#' Possible values are \code{"prominence"} or \code{"distance"}.
#' If \code{rankby = distance}, then one of \code{keyword}, \code{name} or
#' \code{place_type} must be specified. If a \code{search_string} is used then
#' \code{rankby} is ignored.
#' @param keyword \code{string} A term to be matched against all content that
#' Google has indexed for this place, including but not limited to name, type,
#' and address, as well as customer reviews and other third-party content.
#' @param language \code{string} The language code, indicating in which language
#' the results should be returned, if possible. Searches are also biased to the
#' selected language; results in the selected language may be given a higher ranking.
#' See the list of supported languages and their codes
#' \url{https://developers.google.com/maps/faq#languagesupport}.
#' @param name \code{string} \code{vector} One or more terms to be matched against
#' the names of places. Ignored when used with a \code{search_string}. Results will
#' be restricted to those containing the passed \code{name} values. Note that a
#' place may have additional names associated with it, beyond its listed name.
#' The API will try to match the passed name value against all of these names.
#' As a result, places may be returned in the results whose listed names do not
#' match the search term, but whose associated names do.
#' @param place_type \code{string} Restricts the results to places matching the
#' specified type. Only one type may be specified. For a list of valid types,
#' please visit \url{https://developers.google.com/places/supported_types}.
#' @param price_range \code{numeric} \code{vector} Specifying the minimum and
#' maximum price ranges. Values range between 0 (most affordable) and 4 (most expensive).
#' @param open_now \code{logical} Returns only those places that are open for
#' business at the time the query is sent. Places that do not specify opening
#' hours in the Google Places database will not be returned if you include this
#' parameter in your query.
#' @param page_token \code{string} Returns the next 20 results from a previously
#' run search. Setting a \code{page_token} parameter will execute a search with
#' the same parameters used in a previous search. All parameters other than
#' \code{page_token} will be ignored. The \code{page_token} can be found in the
#' result set of a previously run query.
#' @param simplify \code{logical} - TRUE indicates the returned JSON will be coerced into a list. FALSE indicates the returend JSON will be returned as a string
#' into a list.
#' @param curl_proxy a curl proxy object
#' @param key \code{string} A valid Google Developers Places API key.
#'
#' @export
tidy_google_places <- function(search_string = NULL, location = NULL, radius = NULL,
                               rankby = NULL, keyword = NULL, language = NULL, name = NULL,
                               place_type = NULL, price_range = NULL, open_now = NULL,
                               page_token = NULL, simplify = TRUE, curl_proxy = NULL,
                               key = get_api_key("places")) {

  # Call googleway::google_places
  unformatted_result <- googleway::google_places(search_string , location , radius ,
                           rankby , keyword , language , name ,
                           place_type , price_range , open_now ,
                           page_token , simplify, curl_proxy ,
                           key)

  # Unlist this one level
  unlisted_result <- unformatted_result %>% unlist(recursive = F)
  num_results <- length(unlisted_result$results.id)

  # Create a blank tibble for structured results
  google_results_flattened <- tibble()

  for(i in 1:num_results) {
    place_id <- unlisted_result$results.place_id[[i]]
    place_name <- unlisted_result$results.name[[i]]

    # Create a new dummy row
    new_row <- tibble::tibble(place_id = place_id,
                              place_name = place_name,
                              result_number = i,
                              n_results = num_results)

                      # address = address,
                      # latitude = latitude,
                      # longitude = longitude,
                      # types = types,
                      # price_level = price_level,
                      # rating = rating,
                      # permanently_closed = permanently_closed,
                      # result_number = j,
                      # n_results = n_results)

    # Bind this new row into our structured dataset
    google_results_flattened %<>% bind_rows(new_row)
  }

        # # Extract the key fields where they exist
        # account_name <- ifelse(is.null(store_results$name[[j]]), NA, store_results$name[[j]])
        # address <- ifelse(is.null(store_results$formatted_address[[j]]), NA, store_results$formatted_address[[j]])
        # latitude <- ifelse(is.null(store_results$geometry$location$lat[[j]]), NA, store_results$geometry$location$lat[[j]])
        # longitude <- ifelse(is.null(store_results$geometry$location$lng[[j]]), NA, store_results$geometry$location$lng[[j]])
        # rating <- ifelse(is.null(store_results$rating[[j]]), NA, store_results$rating[[j]])
        # price_level <- ifelse(is.null(store_results$price_level[[j]]), NA, store_results$price_level[[j]])
        # permanently_closed <- ifelse(is.null(store_results$permanently_closed[[j]]), NA, store_results$permanently_closed[[j]])
        # types <- ifelse(is.null(paste(store_results$types[[j]], collapse = ";")), NA, paste(store_results$types[[j]], collapse = ";"))
    # place_ids <- googleway::access_result(unformatted_result, "place") %>% tibble::enframe(name = NULL, value = "place_id")
    # place_ids <- googleway::access_result(unformatted_result, "place_name") %>% tibble::enframe(name = NULL, value = "place_name")
    # place_ids <- googleway::access_result(unformatted_result, "coordinates") %>% tibble::as_tibble()
    #
    # googleway::access_result(x, "place_name")

  google_results_flattened
}
