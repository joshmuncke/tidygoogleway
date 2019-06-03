test_that("Seach text must be provided", {
  expect_error(get_tidy_google_place(), "You must provide a search term to Google Places")
})

test_that("Cannot provide a single latitude or longitude only", {
  expect_error(get_tidy_google_place("ralphs","marina del rey",33.981293), "Cannot provide single geolocation co-ordinate")
})

test_that("Cannot provide a single latitude or longitude only", {
  expect_error(get_tidy_google_place("ralphs","marina del rey",NULL, -118.440031), "Cannot provide single geolocation co-ordinate")
})
