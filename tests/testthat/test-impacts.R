test_that("`impacts()` returns a tibble", {
  result <- impacts()
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
})

test_that("`impacts()` returns correct structure", {
  result <- impacts()
  expect_equal(nrow(result), 2)
  expect_equal(names(result), c("impact", "impact_code"))
})

test_that("`impacts()` returns correct impact names", {
  result <- impacts()
  expected_impacts <- c("high", "low")
  expect_equal(result$impact, expected_impacts)
})

test_that("`impacts()` returns correct impact codes", {
  result <- impacts()
  expected_codes <- c("hi", "lo")
  expect_equal(result$impact_code, expected_codes)
})

test_that("`impacts()` columns have correct types", {
  result <- impacts()
  expect_type(result$impact, "character")
  expect_type(result$impact_code, "character")
})
