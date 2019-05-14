
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidygoogleway
=============

The goal of tidygoogleway is to ...

Installation
------------

``` r
  newdata <- tibble::tribble(~acc_name, ~acc_address,
                     "Areal", "Santa Monica, CA 90405",
                     "Ralphs", "Marina Del Rey, CA 90292",
                     "7Eleven", "Lincoln, Santa Monica",
                     "nothing location", "marina del rayyy")
  
 tidy_google_places()
  newdata %>% 
    group_by(id = row_number()) %>% 
    nest() %>% 
    mutate(d = map(data, ~tidy_google_places(.x$acc_name, .x$acc_address, key = my_key, keep_top = F))) %>%
    unnest()
```
