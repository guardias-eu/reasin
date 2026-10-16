test_that("`regions()` returns a tibble", {
  result <- regions()
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
})

test_that("`regions()` returns correct structure", {
  result <- regions()
  expect_equal(nrow(result), 9)
  expect_equal(names(result), c("region", "region_code"))
})

test_that("`regions()` returns correct region names", {
  result <- regions()
  expected_regions <- c(
    "Canary Islands",
    "Guadeloupe",
    "Martinique",
    "French Guiana",
    "Reunion",
    "Mayotte",
    "Saint Martin",
    "Azores",
    "Madeira"
  )
  expect_equal(result$region, expected_regions)
})

test_that("`regions()` returns correct region codes", {
  result <- regions()
  expected_codes <- c(
    "ES7",
    "FRY1",
    "FRY2",
    "FRY3",
    "FRY4",
    "FRY5",
    "FRY6",
    "PT2",
    "PT3"
  )
  expect_equal(result$region_code, expected_codes)
})

test_that("`regions()` columns have correct types", {
  result <- regions()
  expect_type(result$region, "character")
  expect_type(result$region_code, "character")
})
