
tidygoogleway
=============

The [`googleway`](https://cran.r-project.org/web/packages/googleway/vignettes/googleway-vignette.html#result-accessors) package provides some excellent and highly versatile methods for querying and analyzing data from the Google Maps APIs.

`tidygoogleway` builds on the functionality in googleway with a single purpose - to provide a tidy interface to the [Google Places API](https://developers.google.com/places/web-service/intro). The methods in this package assume that you are starting with a dataframe/tibble of location data that you wish to enrich with data from Google Places.

Installation
------------

You can install tidygoogleway from Github using the following command:

``` r
# You must have devtools installed first
devtools::install_github("joshmuncke/tidygoogleway")
```

Setup
-----

To use this package you'll need a Google Places [API key](https://developers.google.com/places/web-service/get-api-key). tidygoogleway will look for this key in your environment variables or you can pass it in using `key`.

Usage
-----

The `add_google_places` function expects a dataframe with (at the minimum) a field containing the name and address of the locations you wish to add Google Places data to. It will return a dataframe with the relevant Places data appended (i.e. it's pipe-able).

Often a Google Places search will return multiple results. In this instance `add_google_places` function will perform a string similarity comparison on the account name and address between the values you provide and the values returned from Google. If you supply latitude and longitude fields then `add_google_places` will factor a geographic distance into this calculation too.

``` r
# The macdonalds dataframe contains the name and address of 11 McDonalds locations in Los Angeles
mcdonalds %>% 
  add_google_places(name, address, key = my_api_key)
  
  newdata %>% add_tidy_google_places(acc_name, acc_address, key = my_api_key)
```

By default, only the best matching account will be returned (so the number of rows in will be the same as the number of rows out). If you wish to override this behaviour and return multiple results use `.keep_all = T`.
