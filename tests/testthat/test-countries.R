test_that("`countries()` returns a tibble", {
  result <- countries()
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
})

test_that("`countries()` returns correct structure", {
  result <- countries()
  expect_equal(nrow(result), 28)
  expect_equal(names(result), c("country", "country_code"))
})

test_that("`countries()` returns correct country names", {
  result <- countries()
  expected_countries <- c(
    "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
    "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
    "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
    "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
    "Slovenia", "Spain", "Sweden", "UK in respect of Northern Ireland"
  )
  expect_equal(result$country, expected_countries)
})

test_that("`countries()` returns correct country codes", {
  result <- countries()
  expected_codes <- c(
    "AT", "BE", "BG", "HR", "CY", "CZ",
    "DK", "EE", "FI", "FR", "DE", "EL",
    "HU", "IE", "IT", "LV", "LT", "LU",
    "MT", "NL", "PL", "PT", "RO",
    "SK", "SI", "ES", "SE", "XI"
  )
  expect_equal(result$country_code, expected_codes)
})

test_that("`countries()` columns have correct types", {
  result <- countries()
  expect_type(result$country, "character")
  expect_type(result$country_code, "character")
})
